include Nanoc3::Helpers::Blogging
include Nanoc3::Helpers::Tagging
include Nanoc3::Helpers::Rendering

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
  if @item_rep and @item_rep.path == path
    %[<span class="current">#{text}</span>]
  else
    %[<a href="#{path}">#{text}</a>]
  end
end

def article_date(a)
  Time.parse(a[:created_at])
end

def timeago(a)
  d = article_date(a).iso8601.to_s
  %[<abbr class="timeago" title="#{d}">#{d}</abbr>]
end
