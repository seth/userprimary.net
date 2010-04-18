#!/usr/bin/env ruby

require 'ostruct'
require 'rubygems'
require 'httparty'

class Disqus
  include HTTParty

  base_uri "disqus.com/api/"
  format :json

  attr_accessor :api_key, :forum_id

  def initialize(api_key, forum_id)
    @api_key = api_key
    @forum_id = forum_id
  end

  def forum_list
    structify(self.class.get("/get_forum_list", :query => {
                               :user_api_key => @api_key
                             })["message"])
  end

  def forum_api_key
    @forum_api_key ||= self.class.get("/get_forum_api_key", :query => {
                                        :user_api_key => @api_key,
                                        :forum_id => self.forum_id
                                      })["message"]
  end

  def forum_posts
    self.class.get("/get_forum_posts", :query => {
                               :forum_id => self.forum_id,
                               :forum_api_key => self.forum_api_key
                             })["message"]
  end

  def thread_list(limit=25)
    structify(self.class.get("/get_thread_list", :query => {
                               :forum_id => self.forum_id,
                               :forum_api_key => self.forum_api_key,
                               :limit => limit
                             })["message"])
  end

  def thread_by_url(url)
    self.class.get("/get_thread_by_url", :query => {
                               :forum_api_key => self.forum_api_key,
                               :url => url
                             })["message"]
  end

  def post_count_for_threads(*thread_ids)
    self.class.get("/get_num_posts", :query => {
                               :thread_ids => thread_ids.join(','),
                               :forum_api_key => self.forum_api_key
                             })["message"]
  end

  def get_thread_posts(thread_id)
    structify(self.class.get("/get_thread_posts", :query => {
                               :forum_api_key => self.forum_api_key,
                               :thread_id => thread_id
                             })["message"])
  end

  def get_or_create_by_url(url, title)
    r = thread_by_url(url)
    if !r
      r = thread_by_identifier(url, title).thread
      update_thread(r["id"], { "url" => url })
    end
    r["id"]
  end

  # opts are:
  # * title
  # * allow_comments
  # * slug
  # * url
  def update_thread(thread_id, opts={})
    body = opts.merge({
                        :forum_api_key => self.forum_api_key,
                        :thread_id => thread_id
                      })
    OpenStruct.new(self.class.post("/update_thread/", :body => body))
  end

  def thread_by_identifier(identifier, title=nil)
    OpenStruct.new(self.class.post("/thread_by_identifier/", :body => {
                                :identifier => identifier,
                                :title => title,
                                :forum_api_key => self.forum_api_key
                              })["message"])
  end

  def post_date(datetime)
    # 2009-03-30T15:41
    if datetime.is_a? String
      datetime = Time.parse(datetime)
    end
    datetime.utc.strftime('%Y-%m-%dT%H:%M')
  end

  def create_comment(comment, tid)
    self.class.post("/create_post/",
                    :body => {
                      "thread_id" => tid,
                      "message" => comment["message"],
                      "author_name" => comment["name"],
                      "author_email" => comment["email"],
                      "forum_api_key" => self.forum_api_key,
                      "created_at" => post_date(comment["date"]),
                      "state" => "approved"
                    })
  end

  def structify(thing)
    if thing
      thing.map do |x|
        x["ID"] = x["id"]
        x.delete("id")
        OpenStruct.new(x)
      end
    else
      nil
    end
    
  end
end
