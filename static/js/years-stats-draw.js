            $.getJSON( "/static/js/years-stats.json").done(function( data ) {
        $(function () {
            $("#gcontainerX01").highcharts({
                title: {
                    text: ''
                },
                yAxis: {
                    title: {
                        text: ''
                    },
                    plotLines: [{
                        value: 0,
                        width: 1,
                        color: '#808080'
                    }]
                },
                legend: {
                    layout: 'vertical',
                    align: 'right',
                    verticalAlign: 'middle',
                    borderWidth: 0
                },
                series: data
            });
        });
            });
