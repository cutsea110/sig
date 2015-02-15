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
	    navigator: {
		enabled: true,
		series: {
		    id: 'navigator'
		}
	    },
	    plotOptions: {
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
                offset: 0,
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
            }, {
                labels: {
                    align: 'left',
                    x: 3
                },
                title: {
                    text: 'Reference'
                },
                height: '60%',
                offset: 25,
                lineWidth: 2
            }, {
                labels: {
                    align: 'left',
                    x: 3
                },
                title: {
                    text: 'Reference'
                },
                top: '65%',
                height: '35%',
                offset: 25,
                lineWidth: 2
            }]
	};

    var chart = new Highcharts.StockChart(opt);
    var api = new RestsigApi('http://localhost/api');


    // Autocomplete for code
    $('.stock-code').autocomplete({
        source: function(req,resp) {
            api.Brands.byLike('%' + req.term + '%').get()
		.done(
		    function(data) {
			resp(data.map(function(b){
			    return {label: b.brandName + '(' + b.brandCode + ')', value: b.brandCode};
			}));
		    }
		);
        },
        autoFocus: true,
        delay: 0,
        minLength: 2
    });
    $('#add-ohlc').click(function() {
	var main_code = $($(this).attr('data-main')).val(),
	    refer_code = $($(this).attr('data-refer')).val();
	api.Stocks.byCode(main_code).get()
	    .done(function (data) {
		var newSeries = {
		    type: 'candlestick',
		    color: '#0101df',
		    upColor: '#df013a',
		    name: data.brand.name,
		    data: data.prices.map(
			function(s) {
			    return [s.date, s.open, s.high, s.low, s.close];
			}),
		    yAxis: 0
		};
		var ret = chart.addSeries(newSeries);
		var nav = chart.get('navigator');
		nav.setData(newSeries.data);
		
		chart.addSeries({
		    type: 'column',
		    color: '#ff8000',
		    name: 'Volume',
		    data: data.prices.map(
			function(s) {
			    return [s.date, s.volume];
			}),
		    yAxis: 1
		});
	    });
	api.Stocks.byCode(refer_code).get()
	    .done(function (data) {
		chart.addSeries({
		    type: 'candlestick',
		    color: '#cee3f6',
		    upColor: '#f5a9e1',
		    lineColor: '#d8d8d8',
		    name: data.brand.name,
		    data: data.prices.map(
			function(s) {
			    return [s.date, s.open, s.high, s.low, s.close];
			}),
		    yAxis: 2
		});
		chart.addSeries({
		    type: 'column',
		    color: '#cecef6',
		    name: 'Volume',
		    data: data.prices.map(
			function(s) {
			    return [s.date, s.volume];
			},false,true),
		    yAxis: 3
		});
	    });
    });

    $('#add-sma-term').click(function() {
        var cd = $('#sma-code').val(),
	    keys = {1: 'n', 2: 's', 3: 'm', 4: 'l', 5: 'xl'},
	    terms = {},
	    colors = {},
	    opts = {type: 'json'};

	$.each(keys, function (k, v) {
	    terms[v] = Number($('#sma-term-' + v).val());
	    colors[v] = $('#sma-color-' + v).val();
	    if (terms[v] > 0) {
		opts[v] = terms[v];
	    }
	});
	
        api.Stocks.byCode(cd).Indicator.sma().get(null, null, opts)
	    .done(
		function(data) {
		    for(var i=1; i<=5; i++) {
			if (data['tl'+i] != null) {
			    chart.addSeries({
				type: 'line',
				name: data['tl'+i].label,
				data: data['tl'+i].ticks.map(function(s){return [s.k, s.v]}),
				color: colors[keys[i]],
				yAxis: 0
			    }, false, true);
			}
		    }
		    chart.redraw();
		}
	    );
    });
	
    $('#rm-last-sma-term').click(function() {
	var lastIdx = chart.series.length;
	chart.series[lastIdx-1].remove();
    });

    $('.color').colorpicker();
    
});
