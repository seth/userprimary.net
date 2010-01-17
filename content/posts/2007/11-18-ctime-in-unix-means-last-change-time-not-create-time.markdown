I've always thought that "<strong>ctime</strong>" provides the creation time of a file on a unix filesystem, and I've always been wrong about that.  A better mnemonic is <em>change time</em> since the ctime indicates the last time a file's metadata (inode) was changed.  It isn't as if this information is deeply hidden.  Indeed, if you read the man page for stat, you will likely find a fairly straight forward description.  For example, on OS X you will see:

<quickcode:noclick>
    # cut from "man stat"

    st_ctime     Time when file status was last changed (inode data modifica-
                 tion).  Changed by the chmod(2), chown(2), link(2),
                 mknod(2), rename(2), unlink(2), utimes(2) and write(2) sys-
                 tem calls.
</quickcode>

The <a href="http://man-wiki.net/index.php/2:stat">Linux man page for stat</a> summarizes ctime behavior nicely: "The  field  st_ctime is changed by writing or by setting inode information (i.e., owner, group, link count, mode, etc.)."

So if this information is so easy to find, why have I (and I suspect I'm not completely alone on this misconception) been operating with a faulty ctime definition all this time?  I think it is because <a href="" title="we are still collecting confirmatory data on this question">even computer programmers are human</a>.  Our primary mode of understanding the world (and this includes computer systems) is to observe patterns and tell ourselves stories about what is happening and why.  As you can confirm from reading any email help forum, looking and guessing comes naturally to us humans, reading fine manuals does not.  So I saw the pattern atime, ctime, and mtime and I made up a story about access time, creation time (bzzt, wrong), and modification time.  It's a sensible story that happens to be wrong.

I don't think there is anything wrong with the looking and guessing approach -- even for programmers.  In fact, I think good software engineers develop a keen intuition about computer systems and use it to great advantage.  Making guesses and quickly verifying them is often much faster than finding, reading, and absorbing all of the fine manuals out in the world.  But it is important to remember that when things don't work as expected, we should assess our assumptions and RTFM as soon as possible.
