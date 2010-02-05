In <a href="/posts/2009/06/15/writing-a-url-shortening-service-in-erlang-part-1">part 1</a>, we described our first pass at a module that interfaces to Mnesia to provide the core storage API for sherl, our URL shortening service.

Our code, at present, consists of a single file, sherl_db.erl.  Since the url record will be useful to modules that call sherl_db, I extracted the definition into a separate url.hrl file and then setup the files to follow the <a href="http://erlang.org/doc/design_principles/applications.html#7.4">OTP directory structure conventions</a>.  Here's what we have:

<pre>
sherl/
|-- Makefile
|-- ebin/
|-- include/
|   `-- url.hrl
|-- src/
|   `-- sherl_db.erl
`-- test/
</pre>

We are going to use <a href="http://erlang.org/doc/apps/common_test/part_frame.html">Common Test</a> as a testing framework.  Common Test comes with recent versions of Erlang, however, you will need to take an extra step to install the <code>run_test</code> script.  Here's what I did:

<pre lang="bash">
cd /usr/local/lib/erlang/lib/common_test-1.4.1
sudo ./install.sh local
sudo ln -s /usr/local/lib/erlang/lib/common_test-1.4.1/priv/bin/run_test \
           /usr/local/bin/run_test
</pre>

Test files go in the test directory of the application and the Common Test (ct) convention is to create a file <code>test/foo_SUITE.erl</code> if you have a module named <code>foo</code> in your application.  Tests in ct are just erlang functions that can do whatever you want.  A successful test function is one that doesn't blow up.  Common Test was not designed as a unit testing framework, but you can use it as one.  There is not, however, a collection of assert functions as one might expect when thinking of a traditional xUnit environment.  If that is what you are looking for, have a look at EUnit.  Personally, I don't miss the assert functions as Erlang's pattern matching makes for a powerful and light weight assertion mechanism.

Below is a minimal <code>sherl_db_SUITE.erl</code>.  Some key features of a ct suite file are:

<ul>
<li>You must include ct.hrl.
<li>Your life will be easier if you specify export_all.
<li>You can define suite-wide test configuration in the suite() function.  Below I've set a time limit of ten seconds for any test.
<li>Setup and cleanup functions for the entire suite can be defined using init_per_suite and end_per_suite.
<li>Tests are executed in the order specified by the <code>all</code> function unless you specify otherwise.  This can be useful to orchestrate test scenarios that depend on sequence.  You can also configure ct to run tests in random order, to repeat tests, and more.
</ul>

<pre lang="erlang">
-module(sherl_db_SUITE).

-compile(export_all).
-include("../include/url.hrl").
-include_lib("ct.hrl").

suite() -> [{timetrap,{seconds,10}}].

init_per_suite(Config) ->
    sherl_db:start([]),
    Config.

end_per_suite(_Config) ->
    sherl_db:stop(),
    ok.

all() -> 
    [unknown_url_is_undefined, roundtrip1, roundtrip2].

%% Test cases start here.

unknown_url_is_undefined(_Config) ->
    undefined = sherl_db:get_url("no-such-url-in-db"),
    ok.

roundtrip1(_Config) ->
    Url1 = "url1",
    Ans1 = sherl_db:get_code(Url1),
    Url1 = Ans1#url.url,
    1 = Ans1#url.code,
    Ans1 = sherl_db:get_url(Ans1#url.code),

    %% same URL yields same record
    Ans1 = sherl_db:get_code(Url1),
    ok.

roundtrip2(_Config) ->
    Url2 = "url2",
    Ans2 = sherl_db:get_code(Url2),
    Url2 = Ans2#url.url,
    2 = Ans2#url.code,
    Ans2 = sherl_db:get_url(Ans2#url.code),
    ok.
</pre>

Here's what this looks like on the shell for a successful run:
<pre lang="bash">
orange sherl (master)$ make test
run_test -pa /Users/seth/Dropbox/SideProj/sherl/lib/sherl/ebin -dir /Users/seth/Dropbox/SideProj/sherl/lib/sherl -logdir ct-results
Erlang R13B (erts-5.7.1) [source] [smp:2:2] [rq:2] [async-threads:0] [kernel-poll:false]


Common Test starting (cwd is /Users/seth/Dropbox/SideProj/sherl/lib/sherl)

Eshell V5.7.1  (abort with ^G)
(ct@orange)1> 
Common Test: Running make in test directories...

CWD set to: "/Users/seth/Dropbox/SideProj/sherl/lib/sherl/ct-results/ct_run.ct@orange.2009-06-18_21.49.33"

TEST INFO: 1 test(s), 6 case(s) in 2 suite(s)

Testing lib.sherl: Starting test, 6 test cases

=INFO REPORT==== 18-Jun-2009::21:49:35 ===
    application: mnesia
    exited: stopped
    type: temporary
Testing lib.sherl: TEST COMPLETE, 6 ok, 0 failed of 6 test cases

Updating /Users/seth/Dropbox/SideProj/sherl/lib/sherl/ct-results/index.html... done
Updating /Users/seth/Dropbox/SideProj/sherl/lib/sherl/ct-results/all_runs.html... done
</pre>

I would like this setup much better if there was a way to have the console output be much more concise.  I find the output as it is a bit difficult to read.  The HTML reports that ct creates harken back to ye olde times in terms of the web design, but nevertheless provide nice summary and detail views of the tests.  Another interesting feature of ct is that it saves a separate report for each run so you can review progress by examining test result history.

[caption id="attachment_200" align="alignright" width="150" caption="ct summary page"]<img src="/uploads/2009/06/Picture-1-150x150.png" alt="ct summary page" title="ct summary page" width="150" height="150" class="size-thumbnail wp-image-200" />[/caption]

Now back to our regularly scheduled program.  We've put in place a few very simple tests to verify that sherl_db:get_code and sherl_db:get_url work properly.  One thing we haven't tested is what happens when requests come in concurrently.  We'll define a test <code>concurrent_creating</code> that will spawn a number of processes each of which will call get_code for the same list of URLs.

<pre lang="erlang">
concurrent_creating(_Config) ->
    NumClients = 5,
    Seq = lists:map(fun erlang:integer_to_list/1, lists:seq(1, 10)),
    Parent = self(),
    F = fun() ->
                Codes = lists:map(fun(N) ->
                                          sherl_db:get_code("http://" ++ N)
                                  end,
                                  Seq),
                Parent ! {self(), Codes}
        end,
    Pids = lists:map(fun(_X) -> spawn(F) end, lists:seq(1, NumClients)),
    Results = [ simple_gather(Pid) || Pid <- Pids ],
    Codes = [ X#url.code || X <- hd(Results) ],
    ExpectedCodes = lists:seq(3, 12),
    lists:foreach(fun(L) ->
                          ExpectedCodes = [X#url.code || X <- L]
                  end,
                  Results),
    ok.

simple_gather(Pid) ->
    receive
        {Pid, Val} ->
            Val
    end.
</pre>

In the test, we define a fun, F, that uses lists:map to obtain the code for each URL in Seq and then sends the result to the parent process.  The parent process spawns a process that runs F for each pseudo-client and then gathers the result by waiting for messages with the expected PIDs.  The result we expect is that each client obtains the same codes for the URLs and that those codes should be in order.  The ordering is perhaps debateable, but I've left it in for now.

Running this test the first time was very satisfying because it completely failed.
<pre lang="bash">
Testing lib.sherl: Starting test, 6 test cases

=ERROR REPORT==== 18-Jun-2009::22:07:15 ===
Error in process <0.158.0> on node 'ct@orange' with exit value: {{badmatch,{aborted,{{case_clause,[{url,21,"http://9",{1245,388035,266231}},{url,20,"http://9",{1245,388035,266190}},{url,19,"http://9",{1245,388035,266148}},{url,18,"http://9",{1245,388035,266103}}]},[{sherl_db,'-get_code/1-fun-0-',1},{mnesia_tm,apply_fun,3},{mnesia_tm,execute_transaction,5},{sherl_db... 

- - - - - - - - - - - - - - - - - - - - - - - - - -
sherl_db_SUITE:simple_gather failed on line 101
Reason: timetrap_timeout
- - - - - - - - - - - - - - - - - - - - - - - - - -

Testing lib.sherl: *** FAILED *** test case 6 of 6
Testing lib.sherl: TEST COMPLETE, 5 ok, 1 failed of 6 test cases
</pre>

The HTML report from ct contains more detail.  One nice feature is that it shows you where the error occured and gives you a link to a display of the source code with numbered lines.  I was going to include some more screenshots, but my screen camera is out of film.

So what happened?  Summary: I misunderstood mnesia transactions.  Transactions in mnesia can run concurrently and the developer needs to add explicit read and/or write locking where appropriate.  My initial mental model of transactions in mnesia imagined transactions as occurring serially.  This isn't the case, and for good reason.  Multiple read-only transactions should obviously be able to run concurrently in a reasonable system.

Within a transaction, either a request to obtain a lock will succeed, in which case you have the lock for the remainder of the transaction (there does not seem to be any notion of explicit unlocking) or you fail to get the lock and the entire transaction is backed out and will be retried.  Because transactions may be replayed a number of times, you need to be very careful that code within a transaction is free of side-effects else repeated executions may have undesired effects.

The first fix for the code, then, is to ask for a write lock for the table.  Adding <code>mnesia:lock({table, url}, write)</code> to the top of the transaction code in get_code made the test pass.  Thanks to some discussion on the erlang questions mailing list, I was made aware of another problem.  With the lock in place, we will indeed get the expected behavior that only one request will create a new record for a given URL.  However, if one of the transactions fails because it is unable to get a lock, then it may happen that next_int is called more often than we expect and we will end up skipping integer values in the codes we assign.  This would happen if the call to mnesia:write bombs out.  But since the write lock is acquired before next_int is called, I would expect that a failed lock would bail out of the transaction without executing all of the code.  So this needs some further investigation.

To wrap up here are links to the complete <a href="http://gist.github.com/132434">sherl_db.erl</a> and <a href="http://gist.github.com/132436">sherl_db_SUITE.erl</a> files.  Aside from the above bug fix, I've also made a few small changes to sherl_db to use mnesia:index_read and mnesia:read instead of qlc.


