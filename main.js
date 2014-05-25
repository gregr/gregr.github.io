$(document).ready(function(){
  var nav_offset = $('#nav-panel').offset();
  $(window).scroll(function(){
    var new_offset = {
      'left': nav_offset.left,
      'top': nav_offset.top + $(this).scrollTop()
    };
    $('#nav-panel').offset(new_offset);
  });
});
