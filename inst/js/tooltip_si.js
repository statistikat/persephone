tooltip_si = function() {
    let custom = this.points[0].series.chart.series[0].options.custom,
        index = Math.round(this.x*13)-13,
        month = custom.month[index],
        year = custom.year[index];
    let res = this.points.map(point => {
        return '<b>' + point.series.name + '</b>: ' +
            Highcharts.numberFormat(point.y, 6);
    }).reduce((x, y) => x + '<br/>' + y);
    return '<i>' + month + ' '  + year + '</i><hr style="margin-top: 0px;">' + res;
};
