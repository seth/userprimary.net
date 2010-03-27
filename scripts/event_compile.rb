#!/usr/bin/env ruby

require 'rubygems'
require 'fsevent'

class NanocCompiler < FSEvent
  def on_change(directories)
    puts "Detected change in: #{directories.inspect}"
    system "nanoc3 compile"
  end

  def start
    puts "watching #{registered_directories.join(", ")} for changes"
    super
  end
end

nc = NanocCompiler.new
nc.latency = 0.2
nc.watch_directories %W(#{Dir.pwd}/content #{Dir.pwd}/layouts)
nc.start
