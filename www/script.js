// gets id most recently clicked filled box
document.addEventListener('click', function(e) {
  if (e.target.classList.contains('filled')) {
    e.stopPropagation();
    if (typeof BUTTON_CLICK_COUNT === 'undefined') {
        BUTTON_CLICK_COUNT = 1;
    } else {
        BUTTON_CLICK_COUNT++;
    }
    Shiny.setInputValue('click_filled', BUTTON_CLICK_COUNT + '_' + e.target.id);
  }
});

// refreshes all boxes
shinyjs.fillBoxes = function(params) {
  var defaultParams = {boxes : null};
  params = shinyjs.getParams(params, defaultParams);
  
  const maxBox = params.boxes.length;
  
  if (maxBox > 0) {
    for (let i = 1; i <= maxBox; i++) {
      const boxId = 'week_' + ('0000'+i).slice(-4);
      const boxElement = document.getElementById(boxId);
  
      if (boxElement) {
        boxElement.className = params.boxes[i - 1];
      }
    }
  }
}

// refreshes one box
shinyjs.fillBox = function(params) {
  var defaultParams = {box : null, index : null};
  params = shinyjs.getParams(params, defaultParams);
  
  const boxId = 'week_' + ('0000'+params.index).slice(-4);
  const boxElement = document.getElementById(boxId);

  if (boxElement) {
    boxElement.className = params.box;
  }
}

// sets up shiny date input
document.addEventListener('DOMContentLoaded', function () {
  const dob_btn = document.getElementById('submit_dob');
  
  dob_btn.addEventListener('click', function() {
    let selectYear = document.getElementById('year_input');
    let selectMonth = document.getElementById('month_input');
    let selectDay = document.getElementById('day_input');
    
    var selectedYear = selectYear.value;
    var selectedMonth = parseInt(selectMonth.value) + 1;
    var selectedDay = selectDay.value;
    
    Shiny.setInputValue('dob', (selectedYear + '-' + ('00'+selectedMonth).slice(-2) + '-' + ('00'+selectedDay).slice(-2)));
  });
});

// update date inputs from server
shinyjs.updateDOBInputs = function(params) {
  var defaultParams = {year : null, month : null, day : null};
  params = shinyjs.getParams(params, defaultParams);
  
  let selectYear = document.getElementById('year_input');
  let selectMonth = document.getElementById('month_input');
  let selectDay = document.getElementById('day_input');
  
  selectYear.value = params.year
  selectMonth.value = params.month - 1
  selectDay.value = params.day
  
  var selectedYear = selectYear.value;
  var selectedMonth = parseInt(selectMonth.value) + 1;
  var selectedDay = selectDay.value;

  Shiny.setInputValue('dob', (selectedYear + '-' + ('00'+selectedMonth).slice(-2) + '-' + ('00'+selectedDay).slice(-2)));
}

// sets up date inputs
document.addEventListener('DOMContentLoaded', function () {
  const monthNames = [
    'January', 'February', 'March', 'April', 'May', 'June',
    'July', 'August', 'September', 'October', 'November', 'December'
  ];

  let selectYear = document.getElementById('year_input');
  let selectMonth = document.getElementById('month_input');
  let selectDay = document.getElementById('day_input');
  let currentYear = new Date().getFullYear();

  for (var y = currentYear; y >= 1900; y--) {
    let yearElem = document.createElement('option');
    yearElem.value = y;
    yearElem.textContent = y;
    selectYear.appendChild(yearElem);
  }

  for (var m = 0; m < 12; m++) {
    let monthElem = document.createElement('option');
    monthElem.value = m;
    monthElem.textContent = monthNames[m];
    selectMonth.appendChild(monthElem);
  }

  var d = new Date();
  var month = d.getMonth();
  var year = d.getFullYear();
  var day = d.getDate();

  selectYear.value = year;
  selectYear.addEventListener('change', adjustDays);
  selectMonth.value = month;
  selectMonth.addEventListener('change', adjustDays);

  adjustDays();
  selectDay.value = day;

  function adjustDays() {
    var selectedYear = selectYear.value;
    var selectedMonth = parseInt(selectMonth.value) + 1;
    selectDay.innerHTML = '';

    var days = new Date(selectedYear, selectedMonth, 0).getDate();

    for (var d = 1; d <= days; d++) {
      var dayElem = document.createElement('option');
      dayElem.value = d;
      dayElem.textContent = d;
      selectDay.appendChild(dayElem);
    }
  }
});


// get local date for server
$(document).on('shiny:connected', function() {
  var d = new Date();
  var month = d.getMonth();
  var year = d.getFullYear();
  var day = d.getDate();
  
  Shiny.setInputValue('local_date', (year + '-' + ('00'+(month + 1)).slice(-2) + '-' + ('00'+day).slice(-2)));
});
