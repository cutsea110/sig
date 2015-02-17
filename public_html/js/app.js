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
		enabled: true
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

    $('.stock-code').typeahead(
	{
	    autoselect: true,
	    hint: true,
	    highlight: true,
	    minLength: 2
	},
	{
	    displayKey: 'value',
	    source: function (query, process) {
		api.Brands.byLike('%' + query + '%').get()
		    .done(function (data) {
			process(data.map(function (b) {
			    return {label: b.brandName + '(' + b.brandCode + ')', value: b.brandCode};
			}));
		    });
	    },
	    templates: {
		suggestion: function (b) {
		    return b.label;
		}
	    }
	});
    $('.add-stock').on({
	'typeahead:selected' : function (obj, datum, name) {
	    var self = $(this);
	    api.Stocks.byCode(datum.value).get()
		.done(function (data) {
		    chart.addSeries({
			type: 'candlestick',
			color: self.data('ohlc-color'),
			upColor: self.data('ohlc-upcolor'),
			lineColor: self.data('ohlc-linecolor'),
			name: data.brand.name,
			data: data.prices.map(
			    function(s) {
				return [s.date, s.open, s.high, s.low, s.close];
			    }),
			yAxis: self.data('ohlc-yaxis')
		    });
		    chart.addSeries({
			type: 'column',
			color: self.data('volume-color'),
			name: 'Volume',
			data: data.prices.map(
			    function(s) {
				return [s.date, s.volume];
			    }),
			yAxis: self.data('volume-yaxis')
		    });
		    setTimeout(function() {
			$('input.highcharts-range-selector', '#' + opt.chart.renderTo)
			    .datepicker({
				format: 'yyyy-mm-dd',
				todayBtn: 'linked',
				orientation: 'auto left',
				autoclose: true,
				todayHighlight: true
			    });
		    }, 0);
		});
	},
    });
    
    $('.enable-next').blur(function () {
	var next = $(this).data('enable-next-selector');
	$(next).removeAttr('disabled');
	if ($(next).data('ttTypeahead') != null) {
	    $(next).prev().remove();
	}
	$(next)
	    .refreshClass('stock-code')
	    .refreshClass('add-stock');
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
