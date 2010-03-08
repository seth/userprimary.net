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

def get_tag_items
  @items.find { |i| i.identifier == "/tags/" }.children
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
