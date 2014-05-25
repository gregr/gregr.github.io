$(document).ready(function(){
  var nav_offset = $('#nav-panel').offset();
  var nav_left = $('#nav-panel').css('left');
  $(window).scroll(function(){
    $('#nav-panel').css('position', 'fixed');
    var new_offset = {
      'left': nav_offset.left,
      'top': nav_offset.top + $(this).scrollTop()
    };
    $('#nav-panel').offset(new_offset);
  });
  $(window).resize(function(){
    $('#nav-panel').css('left', nav_left);
    nav_offset.left = $('#nav-panel').offset().left;
  });
});
