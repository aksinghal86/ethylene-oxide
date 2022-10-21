$(document).keyup(function(event) { 
  if ($("#search").is(":focus") && (event.key == "Enter")) { 
    $("#searchSubmit").click();
  }  
});

Shiny.addCustomMessageHandler('move_cam', function( args ) {
  console.log('custom message');
  var map_id = args[0];
  var map_type = 'mapdeck';
  var location = args[1];
  var zoom = args[2];
  var pitch = 10; 
  var bearing = 0; 
  var duration = 3500; 

  md_change_location( map_id, map_type, location, zoom, pitch, bearing, duration, 'fly');
});

