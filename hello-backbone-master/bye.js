//Modelo
const modelo = Backbone.Model.extend({
    defaults{
        texto: ""
    }
});
const coleccion = Backbone.collection.extend({
    Model: modelo
});

//Vista
const vista = Backbone.Marionette.View.extend({
    tagName: "p",
    template: "#userView"
});
const vistas = Backbone.Marionette.CollectionView.extend({
    childView: vista
});

const vista2 = Backbone.Marionette.View.extend({
    template: '#containerView',
    regions: {
        list: '#list'
    },
    initialize: function (options) {
        this.Vista = new vista(
        {
                collection: options.coleccion
        });
    },
        onRender() {
            this.showChildView('list', this.Vista);
    }
});
const App = Backbone.Marionette.Application.extend(
    {
        //zona donde se cargara el codigo
        region: '#content',
        //constructor
        initialize: function (options) {
            this.vista2 = new vista2(options);
            this.showView(this.containerView);
        }
    });
$(function () {
    var app = new App({ coleccion: new coleccion() });
    app.start();
});