#!/usr/bin/env ruby

require "rubygems"
require "httparty"
require "crack"
require File.join(File.dirname(__FILE__), "disqus")

config = YAML.load_file("disqus-key.yaml")
API_KEY = config["key"]
FORUM_ID = config["fid"]

@blog = Disqus.new(API_KEY, FORUM_ID)

# this is the XML export file from Disqus, edited to fix
# URLs
z = Crack::XML.parse(open("comments2.xml").read)
z["articles"]["article"].each do |a|
  title = File.basename(a["url"]).gsub("-", " ")
  tid = @blog.get_or_create_by_url(a["url"], title)
  comments = a["comments"]["comment"]
  if comments.is_a? Hash
    comments = [comments]
  end
  puts "writing comments for:"
  puts a["url"] + " "
  comments.each do |c|
    print "."
    @blog.create_comment(c, tid)
  end
  puts "  done"
end
