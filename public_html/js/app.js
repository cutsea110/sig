$(function() {

    var opt =
	{
            chart: {
		renderTo: 'container'
            },
            credits: {
		enabled: false
            },
            legend: {
		enabled: true
            },
	    plotOptions: {
		candlestick: {
		    color: 'green',
		    upColor: 'red'
		},
		column: {
		    color: 'orange'
		},
		series: {
		    turboThreshold: 365 * 20 // 20 years
		}
	    },
	    rangeSelector: {
		selected: 2
	    },
            title: {
		text: 'Stock Chart'
            },
	    tooltip: {
		valueDecimals: 2
	    },
            yAxis: [{
                labels: {
                    align: 'right',
                    x: -3
                },
                title: {
                    text: 'OHLC'
                },
                height: '60%',
                lineWidth: 2
            }, {
                labels: {
                    align: 'right',
                    x: -3
                },
                title: {
                    text: 'Volume'
                },
                top: '65%',
                height: '35%',
                offset: 0,
                lineWidth: 2
            }]
	};

    var chart = new Highcharts.StockChart(opt);
    var api = new RestsigApi('http://localhost/api');


    // Autocomplete for code
    $('#code').autocomplete({
        source: function(req,resp) {
            api.Brands.byLike('%' + req.term + '%').get(
		function(data) {
                    resp(data.map(function(b){
			return {label: b.brandName + '(' + b.brandCode + ')', value: b.brandCode};
                    }));
		},
		function (err) {
		    alert(err);
		},
		{type: 'json'}
	    )
        },
        select: function(event, ui) {
            var cd = ui.item.value;
            api.Stocks.byCode(cd).get(
                function (data) {
                    chart.addSeries(
			{
			    type: 'candlestick',
			    name: data.brand.name,
			    data: data.prices.map(
				function(s) {
				    return [s.date, s.open, s.high, s.low, s.close];
				}),
			    yAxis: 0
			}
		    );
		    chart.addSeries(
			{
			    type: 'column',
			    name: 'Volume',
			    data: data.prices.map(
				function(s) {
				    return [s.date, s.volume];
				}),
			    yAxis: 1
			}
		    );
                },
                function (err) {
                    alert(err);
                },
                {type: 'json'}
            );
        },
        autoFocus: true,
        delay: 0,
        minLength: 2
    });

    $('#add-sma-term').click(function() {
        var cd = $('#code').val(),
	    ps = $('#sma-term').val().trim().split(/[ ,]/)
	    .map(function (n) {return parseInt(n);})
	    .filter(function(n){return !isNaN(n) && n > 0;}),
	    keys = ["n", "s", "m", "l", "xl"],
	    opts = {type: 'json'};
	for (var i = 0; i < ps.length && i < keys.length; i++) {
	    opts[keys[i]] = ps[i];
	}

        api.Stocks.byCode(cd).Indicator.sma().get(
	    function(data) {
		for(var i=1; i<=5; i++) {
		    if (data['tl'+i] != null) {
			chart.addSeries({
			    type: 'line',
			    name: data['tl'+i].label,
			    data: data['tl'+i].ticks.map(function(s){return [s.k, s.v]}),
			    yAxis: 0
			}, false, true);
		    }
		}
		chart.redraw();
	    },
	    function(err){
		alert(err);
	    },
	    opts
	);
    });

    $('#rm-last-sma-term').click(function() {
	var lastIdx = chart.series.length;
	chart.series[lastIdx-1].remove();
    });

});
