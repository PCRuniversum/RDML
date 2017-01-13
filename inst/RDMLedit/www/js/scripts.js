addCellOnClick = function(){
  $('table').mousedown(function (event) {
    if (event.ctrlKey) {
        event.preventDefault();
    }
  });
  $('th').single_double_click(function () {
    Shiny.onInputChange("cellClick", [Math.random(), this.id, "click", ""]);
  }, function () {
    Shiny.onInputChange("cellClick", [Math.random(), this.id, "dblclick", ""]);
  });
  $('td').single_double_click(function (event) {
    if (event.ctrlKey) {
      Shiny.onInputChange("cellClick", [Math.random(), this.id, "click", "ctrl"]);
    } else {
      Shiny.onInputChange("cellClick", [Math.random(), this.id, "click", ""]);
    }
  }, function (event) {
    if (event.ctrlKey) {
      Shiny.onInputChange("cellClick", [Math.random(), this.id, "dblclick", "ctrl"]);
    } else {
      Shiny.onInputChange("cellClick", [Math.random(), this.id, "dblclick", ""]);
    }
  });
};