$( document ).ready(function() {
  $( ".navbar .container-fluid" ).append( '<img src="rcglogo.png" align="right">' );
});

shinyBS.addTooltip = function(id, type, opts) {
  var $id = shinyBS.getTooltipTarget(id);
  var dopts = {html: true};
  opts = $.extend(opts, dopts);

  if(type == "tooltip") {
    $id.tooltip("destroy");
    setTimeout(function() {$id.tooltip(opts);},200);
  } else if(type == "popover") {
    $id.popover("destroy");
    setTimeout(function() {$id.popover(opts);},200);
  }
};