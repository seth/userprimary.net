include UserPrimary::Helpers::Blogging
include Nanoc3::Helpers::Tagging
include Nanoc3::Helpers::Rendering
include Nanoc3::Helpers::XMLSitemap

require 'rubygems'
require 'hpricot'
require 'time'

# sort items with a :created_at timestamp in
# reverse chronological order
def sort_posts_by_date(posts)
  posts.sort_by { |p| Time.parse(p[:created_at]) }.reverse
end

def sorted_items_with_tag(tag_name)
  sort_posts_by_date(items_with_tag(tag_name))
end

def get_tag_items
  tag_items = @items.find { |i| i.identifier == "/tags/" }.children
  tag_items.sort { |a, b| a[:tag_name].downcase <=> b[:tag_name].downcase }
end

def get_archive_items
  aa = @items.find { |i| i.identifier == "/archives/" }.children
  aa = aa.sort { |a, b| b[:most_recent] <=> a[:most_recent] }
  m = Hash.new { |h, k| h[k] = [] }
  aa.each do |a|
    m[a[:year]] << a
  end
  ans = []
  m.keys.sort.reverse.each do |k|
    ans << { :year => k, :items => m[k] }
  end
  ans
end

def nav_link_to_unless_current(text, path)
  lb = %[<strong class="bracket">{</strong>]
  rb = %[<strong class="bracket">}</strong>]
  if @item_rep && ((@item_rep.path == path) ||
    (path.length > 1 && @item_rep.path =~ /^#{path[1..-1]}/))
    %[<span class="current">#{lb}#{text}#{rb}</span>]
  else
    %[<a href="#{path}">#{lb}#{text}#{rb}</a>]
  end
end

def article_date(a)
  Time.parse(a[:created_at])
end

def timeago(a)
  d = article_date(a).iso8601.to_s
  %[<abbr class="timeago" title="#{d}">#{d}</abbr>]
end

def stylesheet(name)
  %[<link rel="stylesheet" type="text/css" href="/style/#{name}"></link>]
end

def javascript(name)
  %[<script type="text/javascript" src="/js/#{name}"></script>]
end

def feed_link
  url = "http://userprimary.net/user/feed.atom"
  ct = "application/atom+xml"
  title = "User Primary"
  %[<link rel="alternate" type="#{ct}" title="#{title}" href="#{url}"></link>]
end
## %[<link rel='index' title='User Primary' href='http://userprimary.net/user' />]

def linked_in_badge1
  %[<a href="http://www.linkedin.com/in/sethfalcon" >
          <img src="http://www.linkedin.com/img/webpromo/btn_viewmy_160x25.png" width="160" height="25" border="0" alt="View Seth Falcon's profile on LinkedIn">
    </a>]
end

def linked_in_badge
  %[<a href="http://www.linkedin.com/in/sethfalcon" >
          <img src="http://www.linkedin.com/img/webpromo/btn_profile_greytxt_80x15.png" width="80" height="15" border="0" alt="View Seth Falcon's profile on LinkedIn">
    </a>]
end

def twitter_badge
  %[<a href="http://www.twitter.com/sfalcon"><img src="http://twitter-badges.s3.amazonaws.com/t_small-a.png" alt="Follow sfalcon on Twitter"/></a>]
#  %[<a href="http://www.twitter.com/sfalcon"><img src="http://twitter-badges.s3.amazonaws.com/follow_me-b.png" alt="Follow sfalcon on Twitter"/></a>]
end

def feed_badge
  %[<a href="/feed.atom" title="actually, it's an atom feed">RSS feed <img src="/images/feed-icon-14x14.png" /></a>]
end

def meta_tag(name, content)
  %[<meta content="#{content}" name="#{name}"></meta>]
end

def site_meta_tags
  meta = {
    "robots" => "INDEX,FOLLOW",
    "content-language" => "english",
    "keywords" => ("Seth Falcon, user primary, userprimary, " +
                   "userprimary.net, R programming"),
    "description" => "Homepage and blog for Seth Falcon, User Primary",
    "author" => "Seth Falcon",
    "copyright" => "Seth Falcon",
    "audience" => "All"
  }
  meta.map do |name, content|
    meta_tag(name, content)
  end.join("\n")
end

def openid
  %[<link rel="openid.server" href="http://www.myopenid.com/server" />
<link rel="openid.delegate" href="http://seth.falcon.myopenid.com" />]
end

def delicious_widget
  %[<script type="text/javascript" src="http://feeds.delicious.com/v2/js/sethf?title=Delicious%20Bookmarks&count=5&sort=date&extended&name"></script>]
end

def ga_tracking
  script = <<EOF
<script type="text/javascript">
var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");
document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));
</script>
<script type="text/javascript">
try {
var pageTracker = _gat._getTracker("UA-15243104-1");
pageTracker._trackPageview();
} catch(err) {}</script>
EOF
  script
end

def disqus_dev_mode?
  File.exist?("DEV_MODE") ? 1 : 0
end

def disqus_embed_disable_comments
  embed = <<EOF
<p>Comments are temporarily disabled.  If you'd like to share your thoughts, send me a note on Twitter (@sfalcon) or send me an email (contact details below).</p>
EOF
end

def disqus_embed
  embed = <<EOF
<div id="disqus_thread"></div>
<script type="text/javascript">
  /**
    * var disqus_identifier; [Optional but recommended: Define a unique identifier (e.g. post id or slug) for this thread] 
    */
  (function() {
   var dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;
   dsq.src = 'http://userprimary-net.disqus.com/embed.js';
   (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
  })();
</script>
<noscript>Please enable JavaScript to view the <a href="http://disqus.com/?ref_noscript=userprimary-net">comments powered by Disqus.</a></noscript>
<a href="http://disqus.com" class="dsq-brlink">blog comments powered by <span class="logo-disqus">Disqus</span></a>
EOF
  embed
end

def make_excerpt(post)
  doc = Hpricot(post.reps.first.content_at_snapshot(:last))
  excerpt = doc/"#main p"
  excerpt.inner_text[0..250] + "... "
end

def recent_article_pairs(n)
  arts = []
  sa = sorted_articles[0..(n-1)]
  (0..(n - 1)).select { |i| i % 2 == 0 }.each do |i|
    arts << [sa[i], sa[i + 1]]
  end
  arts
end

def add_missing_info
  items.each do |item|
    if item[:file]
      # nanoc3 >= 3.1 will have this feature, add for older versions
      item[:extension] ||= item[:file].path.match(/\..*$/)[0]
    end
  end
end

def create_tags_pages
  tags = Hash.new { |h, k| h[k] = 0 }
  items.each do |item|
    if item[:kind] == "article"
      if item[:tags]
        item[:tags].each { |t| tags[t] += 1 }
      end
    end
  end

  tags.each do |tag, count|
    content = %[= render('tag', :tag_name => "#{tag}", :tag_count => "#{count}")]
    items << Nanoc3::Item.new(content,
                              { :title => "#{tag}",
                                :tag_name => tag,
                                :tag_count => count.to_s
                              },
                              "/tags/#{tag}/")
  end
end

def create_month_archives
  bymonth = Hash.new { |h, k| h[k] = [] }
  items.each do |item|
    if item[:kind] == "article"
      t = Time.parse(item[:created_at])
      bymonth[t.strftime("%Y-%B")] << item
    end
  end

  bymonth.each do |year_month, posts|
    posts = posts.sort_by { |p| Time.parse(p[:created_at]) }.reverse
    most_recent = Time.parse(posts.first[:created_at])
    items << Nanoc3::Item.new(render('bymonth', :posts => posts,
                                     :year_month => year_month),
                              { :title => "#{year_month}",
                                :post_count => posts.count,
                                :most_recent => most_recent,
                                :month => most_recent.strftime("%B"),
                                :year => most_recent.strftime("%Y")
                              },
                              "/archives/#{year_month}/")
  end
end
