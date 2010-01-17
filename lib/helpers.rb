include Nanoc3::Helpers::Blogging
include Nanoc3::Helpers::Tagging
include Nanoc3::Helpers::Rendering

require 'time'

# sort items with a :created_at timestamp in
# reverse chronological order
def sort_posts_by_date(posts)
  posts.sort_by { |p| Time.parse(p[:created_at]) }.reverse
end

