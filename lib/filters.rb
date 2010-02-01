class PreFix < Nanoc3::Filter
  identifier :prefixhack
  def run(content, params={})
    # For generated HTML with indenting, the idea is to find the first
    # <pre> tag and see how much it is indented and to trip this much
    # leading white space from all lines.  This is only going to work
    # if the indent doesn't change.
    #
    # Leave content unchanged if no <pre> tags are found.
    indent = content.grep(/^ *<pre/).map { |s| s.index("<pre") }.first
    if indent
      regex = "^ {#{indent}}"
      content.gsub(/#{regex}/, "")
    else
      content
    end
  end
end
