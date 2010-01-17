Last week Evri launched its public beta.  No password, no sign-up.  You are just a click away from checking out our take on searching less and understanding more: <a href="http://www.evri.com" title="Search less, understand more">http://www.evri.com</a>.

We also have made available what we call "the widget" that allows anyone to get Evri content recommendations on their own website.  If you are reading this on the <a href="http://userprimary.net/user/">User Primary site</a>, you should see a small Evri icon at the bottom of this post.  Click it, and you will get an overlay that will show related articles, people, places, and things.

Installing this on my Wordpress powered blog was fairly simple.  There are instructions on Evri's <a href="http://www.evri.com/partners-and-bloggers.html">partners and bloggers page</a> but if you are running a recent version of Wordpress, you can install the widget using the admin screens of your blog.  Here's how I installed the widget using Wordpress version 2.6.2.

1. Log in to the Site Admin for your blog and click on <strong>Design</strong>, then <strong>Theme Editor
</strong><a href="http://userprimary.net/user/wp-content/uploads/2008/09/200809281531.jpg" onclick="window.open('http://userprimary.net/user/wp-content/uploads/2008/09/200809281531.jpg','popup','width=268,height=83,scrollbars=no,resizable=yes,toolbar=no,directories=no,location=no,menubar=no,status=yes,left=0,top=0');return false"><img src="http://userprimary.net/user/wp-content/uploads/2008/09/200809281531-tm.jpg" height="100" width="322" border="1" hspace="4" vspace="4" alt="200809281531" /></a>
2. On the right, click on <strong>Footer (footer.php)</strong>
<a href="http://userprimary.net/user/wp-content/uploads/2008/09/200809281532.jpg" onclick="window.open('http://userprimary.net/user/wp-content/uploads/2008/09/200809281532.jpg','popup','width=160,height=119,scrollbars=no,resizable=yes,toolbar=no,directories=no,location=no,menubar=no,status=yes,left=0,top=0');return false"><img src="http://userprimary.net/user/wp-content/uploads/2008/09/200809281532-tm.jpg" height="100" width="134" border="1" hspace="4" vspace="4" alt="200809281532" /></a>
3. Now in another browser window, go Evri's <a href="http://blog.evri.com/index.php/widget-wordpress/">wordpress-widget page</a> and copy the code to your clipboard.

4. Find the line at the bottom of footer.php that looks like: &lt;?php wp_footer(); ?&gt; and <em>paste the code you copied below this line</em>.

<a href="http://userprimary.net/user/wp-content/uploads/2008/09/200809281534.jpg" onclick="window.open('http://userprimary.net/user/wp-content/uploads/2008/09/200809281534.jpg','popup','width=392,height=126,scrollbars=no,resizable=yes,toolbar=no,directories=no,location=no,menubar=no,status=yes,left=0,top=0');return false"><img src="http://userprimary.net/user/wp-content/uploads/2008/09/200809281534-tm.jpg" height="100" width="311" border="1" hspace="4" vspace="4" alt="200809281534" /></a>

5. <strong>Save/update</strong> the footer page.
Note that if you use a caching plugin like WP Super Cache, you need to clear your cache to get the widget working on your site.  You can do this via the WP Super Cache plugin configuration screen.

<!-- technorati tags start --><p style="text-align:right;font-size:10px;">Technorati Tags: <a href="http://www.technorati.com/tag/evri" rel="tag">evri</a></p><!-- technorati tags end -->
