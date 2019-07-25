(function($) {

    var pruebas = Backbone.Marionette.View.extend(
    {
        el : '#UserInput',
        initialize: function () {
            this.helloListView = new HelloListView();
        },
        events: {
            'click button': 'addToHelloCollection'
        },
        addToHelloCollection: function () {
            var hello = new Hello({
                name: this.$('#name').val(),
                age: this.$('#age').val()
            });
            this.helloListView.collection.add(hello);
        }
        });
    var Hello = Backbone.Model.extend(
        {
            initialize: function () {
                this.name = 'name',
                    this.age = 'age'
            }
        });
    var Data = Backbone.Marionette.View.extend(
    {
		tagName : 'li',
		render : function() {
			$(this.el).html('Hello World, my name is ' + this.model.get('name') + ' and I have ' + this.model.get('age') + ' years old');
			return this;
		}
	});
    var HelloList = Backbone.Collection.extend(
    {
		model : Hello
	});
    var HelloListView = Backbone.Marionette.View.extend(
    {
        el: '#HelloList',
		initialize : function() {
			_.bindAll(this, 'render', 'appendToHelloUL');
			this.collection = new HelloList();
			this.collection.bind('add', this.appendToHelloUL);
		},
		render : function(){
			$.each(this.collection.models, function(blablaModel){
				self.appendToHelloUL(blablaModel);
			});
		},
		appendToHelloUL : function(blablaModel) {
			var data = new Data({
				model : blablaModel
			});
			$(this.el).append(data.render().el);
		}
    });
    new pruebas();

})(jQuery);