require 'rubygems'
require 'sequel'
require 'fileutils'
require 'yaml'

# NOTE: This converter requires Sequel and the MySQL gems.
# The MySQL gem can be difficult to install on OS X. Once you have MySQL
# installed, running the following commands should work:
# $ sudo gem install sequel
# $ sudo gem install mysql -- --with-mysql-config=/usr/local/mysql/bin/mysql_config

module BlogConverter
  module WordPress
    def self.process(config_file)
      config = YAML.load_file(config_file)
      user = config['user']
      pass = config['pass']
      dbname = config['dbname']
      prefix = config['prefix']
      host = config['host']

      # Reads a MySQL database via Sequel and creates a post file for
      # each post in wp_posts that has post_status = 'publish'.  This
      # restriction is made because 'draft' posts are not guaranteed
      # to have valid dates.
      posts_q = "select post_title, post_name, post_date, post_content, post_excerpt, ID, guid from #{prefix}_posts where post_status = 'publish' and post_type = 'post'"
      
      tag_q = "SELECT tt.name FROM #{prefix}_terms as tt, #{prefix}_term_relationships as rr WHERE rr.object_id = '%s' AND tt.term_id = rr.term_taxonomy_id"

      post_id_yaml_map = {}
      db = Sequel.mysql(dbname, :user => user, :password => pass, :host => host)

      db[posts_q].each do |post|
        title = post[:post_title]
        slug = post[:post_name]
        date = post[:post_date]
        content = post[:post_content]
        name = "posts/%04d/%02d/%02d/%s" % [date.year, date.month, date.day,
                                            slug]
        post_id_yaml_map[post[:ID]] = "#{name}.yaml"
        # Get the relevant fields as a hash, delete empty fields and
        # convert to YAML metadata file
        data = {
           'title' => title.to_s,
           'excerpt' => post[:post_excerpt].to_s,
           'wordpress_id' => post[:ID],
          'wordpress_url' => post[:guid],
          # for nanoc blogging helper
          'kind' => 'article',
          'created_at' => date.to_s, # is this the right format??
          'tags' => db[tag_q % post[:ID]].map { |t| t[:name] }
         }.delete_if { |k,v| v.nil? || v == ''}.to_yaml

        FileUtils.mkdir_p(File.dirname(name))
        File.open("#{name}.markdown", "w") do |f|
          f.puts content
        end
        File.open("#{name}.yaml", "w") do |f|
          f.puts data
        end
      end
    end
  end
end

if __FILE__ == $0
  BlogConverter::WordPress.process(ARGV[0])
end
