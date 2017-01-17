shinyjs.selectRows = function(params){
  var table = $('#DataTables_Table_0').DataTable();
  table.rows().deselect();
  var pp = "A06"
  var indexes = table.rows().eq( 0 ).filter( function (rowIdx) {
    return $.inArray(table.cell(rowIdx, 1).data(), pp) === -1 ? false : true;
  } );
  //table.rows(indexes).select()
  $( table.rows(indexes).nodes()).addClass( 'select' );
};

shinyjs.filterTblByPositions = function(params){
  var table = $('#qPCRDt').find("table").DataTable();
  table
    .column(4).search(
        params,
        true,
        false
    ).draw();
  table = $('#meltingDt').find("table").DataTable();
  table
    .column(4).search(
        params,
        true,
        false
    ).draw();
};

shinyjs.changeConfirmedStatus = function(params){

  var table = $('#insider_ui-resultsTbl').find("table").DataTable();
  var indexes = table.rows().eq(0).filter( function (rowIdx) {
    return table.cell(rowIdx, 2 ).data() === "<b>".concat(params[1], "</b>") &&
        table.cell(rowIdx, 1 ).data() === params[0]  ? true : false;
    });
    table.cell(indexes, 15).data(params[2]);

    table.cell(indexes, 14).data( params[3]);


};

shinyjs.toggleCatModal = function() {
  $('#myModal').modal('toggle');
};

shinyjs.changeActiveTab = function(){
//Shiny.unbindAll()
if ($( "div[data-value='x1']").hasClass("tab-pane active"))
{
  $('#tabbox1 li').each(function()
  {
    if ($(this).hasClass("active"))
    { $(this).removeClass("active"); }
    else
    { $(this).addClass("active"); }


  });
  $( "div[data-value='x1']").removeClass("tab-pane active").addClass("tab-pane") ;
  $( "div[data-value='x2']").removeClass("tab-pane").addClass("tab-pane active") ;
}
Shiny.onInputChange("doUpdateResults", Math.random());
//Shiny.bindAll()
};
