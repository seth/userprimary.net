$('document').ready(function () {
    $('#header').corner("bottom 8px")
    $('#main').corner("8px")
    $('#summaries').corner("8px")
    $('#footer').corner("top 8px")
    $('#coord_img').corner("6px")
    $('pre').corner("8px")
    $('abbr.timeago').timeago()
})

var disqus_developer = <%= disqus_dev_mode? %>; 
