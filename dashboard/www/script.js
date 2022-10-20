$(document).keyup(function(event) { 
  if ($("#search").is(":focus") && (event.key == "Enter")) { 
    $("#searchSubmit").click();
  }  
});