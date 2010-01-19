include Nanoc3::Helpers::Blogging
include Nanoc3::Helpers::Tagging
include Nanoc3::Helpers::Rendering

require 'time'

# sort items with a :created_at timestamp in
# reverse chronological order
def sort_posts_by_date(posts)
  posts.sort_by { |p| Time.parse(p[:created_at]) }.reverse
end

def nav_link_to_unless_current(text, path)
  if @item_rep and @item_rep.path == path
    %[<span class="current">#{text}</span>]
  else
    %[<a href="#{path}">#{text}</a>]
  end
end

