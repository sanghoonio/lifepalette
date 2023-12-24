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