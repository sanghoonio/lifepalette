$(document).on('click', '.filled', function(e) {
   e.stopPropagation()
   if (typeof BUTTON_CLICK_COUNT == 'undefined') {
      BUTTON_CLICK_COUNT = 1; 
    } else {
      BUTTON_CLICK_COUNT ++;
    }
    Shiny.setInputValue('click_filled', 
      BUTTON_CLICK_COUNT + '_' + e.target.id);
});

shinyjs.fillBoxes = function(params) {
  var defaultParams = {boxes : null};
  params = shinyjs.getParams(params, defaultParams);
  
  const maxBox = params.boxes.length;
  
  if (maxBox > 0) {
    for (let i = 1; i <= maxBox; i++) {
      var formatted_i = ('0000'+i).slice(-4);
      const boxId = 'week_' + formatted_i;
      const boxElement = document.getElementById(boxId);
  
      if (boxElement) {
        boxElement.className = params.boxes[i - 1];
      }
    }
  }
}

shinyjs.fillBox = function(params) {
  var defaultParams = {box : null, index : null};
  params = shinyjs.getParams(params, defaultParams);
  
  var formatted_index = ('0000'+params.index).slice(-4);
  const boxId = 'week_' + formatted_index;
  const boxElement = document.getElementById(boxId);

  if (boxElement) {
    boxElement.className = params.box;
  }
}
