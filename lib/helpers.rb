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
  archives = @items.find { |i| i.identifier == "/archives/" }.children
  archives.sort_by { |a| Time.parse(a[:title]) }
end

def nav_link_to_unless_current(text, path)
  if @item_rep and @item_rep.path == path
    %[<span class="current">#{text}</span>]
  else
    %[<a href="#{path}">#{text}</a>]
  end
end

