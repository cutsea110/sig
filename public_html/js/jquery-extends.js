$(function() {
    
    $.fn.extend({
	refreshClass: function(klass) {
	    if (this.hasClass(klass)) {
		return this.removeClass(klass).addClass(klass);
	    }
	    return this;
	}
    });
    
});
