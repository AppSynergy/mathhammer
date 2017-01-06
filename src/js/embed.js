var node = document.getElementById('app');
var app = Elm.Main.embed(node);
var chart = [];

app.ports.drawChart.subscribe(function(data) {

  var chartId = data[0];
  var chartMetric = data[1];
  var chartData = data[2];

  var chartOptions = {
    legend: { display: false },
    tooltips: { callbacks: {
      title: function(item, data) {
        var label = item[0].xLabel + " " + chartMetric.toLowerCase() + ":";
        return ((item[0].datasetIndex == 0) ? "Exactly " : "At least ") + label;
      },
      label: function(item, data) {
        if (item.yLabel < 0.0001) {
          return "That's one in a million, kid.";
        }
        var percent = 0;
        if (item.yLabel < 0.01) {
          percent = Math.floor(item.yLabel * 10000) / 100;
        } else {
          percent = Math.floor(item.yLabel * 1000) / 10;
        }
        return percent + "%";
      }
    }}
  };

  var canvas = document.getElementById(chartId);
  if (canvas) {
    var ctx = canvas.getContext("2d");
    if (exists = chart[chartId]) {
      exists.destroy();
    }
    chart[chartId] = new Chart(ctx, {
      type: 'bar',
      data: chartData,
      options: chartOptions,
    });
  }

});
