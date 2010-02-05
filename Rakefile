require 'nanoc3/tasks'
require 'yaml'
require 'fileutils'

desc "copy assets to output"
task :copy_assets do
  site_config = YAML.load_file("./config.yaml")
  output_dir = site_config["output_dir"]
  system "rsync -gprt --partial --exclude='.svn' assets/ #{output_dir}"
end

desc "compile"
task :compile do
  system "nanoc3 co"
end

desc "Build userprimary.net"
task :build => [ :compile, :copy_assets ]

desc "Delete output directory contents (warning: uses rm -rf)"
task :real_clean do
  site_config = YAML.load_file("./config.yaml")
  output_dir = site_config["output_dir"]
  FileUtils.rm_rf(output_dir)
  FileUtils.mkdir_p(output_dir)
end

task :default => :build

desc "deploy it call with [true/false] to actually deploy"
task :deploy, [:doit] do |t, args|
  args.with_defaults(:doit => false)
  site_config = YAML.load_file("./config.yaml")
  output_dir = site_config["output_dir"].sub(/\/$/, "")
  config = YAML.load_file("./deploy.yaml")
  user = config["user"]
  host = config["host"]
  root = config["root"]
  dry = args.doit ? "" : "--dry-run"
  cmd = "rsync -av #{dry} #{output_dir}/ #{user}@#{host}:#{root}"
  system "#{cmd}"
  msg = args.doit ? "sync complete" : "DRY RUN finished, rerun with doit=true"
  puts msg
end

task :test1, [:a1] do |t, args|
  puts "a1 is #{args.a1}"
end
