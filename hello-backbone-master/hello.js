    //01 - Modelo de usuario
    const Usuario = Backbone.Model.extend({
        defaults: {
            nombre: "",
            edad: 0
        }
    });
    //02 - Coleccion de modelos
    const Usuarios = Backbone.Collection.extend({
        Model: Usuario
    });

const nameView = Backbone.Marionette.View.extend(
    {
        template: "#name"
    });
    
    //03 - Vista que captura el patron del usuario
    const UsuarioView = Backbone.Marionette.View.extend({
        tagName: "li",
        template: "#userView"
    });

    //05 - Coleccion que coge la vista de usuario (si existe)
    const UsuariosView = Backbone.Marionette.CollectionView.extend({
        //tagName: "p",
        childView: UsuarioView,
    });

    //06 - Vista que captura los datos del formulario
    const FormView = Backbone.Marionette.View.extend({
        //tagName: "div",
        className: "form-view",
        template: "#formView",
        events: {
            'click button': 'crearNuevoUsuario'
        },
        ui: {
            nombre: '#nombre',
            edad: '#edad'
        },
        crearNuevoUsuario: function () {
            this.collection.push({
                nombre: this.ui.nombre.val(),
                edad: this.ui.edad.val()
            });
            this.ui.nombre.val("");
            this.ui.edad.val("");
        }

    });

    //07 - Vista que captura los datos de la plantilla containerview y renderiza las vistas hijas
    const ContainerView = Backbone.Marionette.View.extend(
        {
            //plantilla html
            template: '#containerView',
            //elemento a pintar
            //tagName: "div",
            //zona(s) donde pintara los datos
            regions: {
                form: '#form',
                list: '#list'
            },
            //constructor
            initialize: function (options) {
                this.formView = new FormView({
                    collection: options.usuarios
                });
                this.usuariosView = new UsuariosView({
                    collection: options.usuarios
                });
            },
            //renderizador
            onRender() {
                this.showChildView('form', this.formView);
                this.showChildView('list', this.usuariosView);
            }
        })

    //08 - app principal
    const UsuariosApp = Backbone.Marionette.Application.extend(
        {
            //zona donde se cargara el codigo
            region: '#content',
            //constructor
            initialize: function (options) {
                this.containerView = new ContainerView(options);
                this.showView(this.containerView);
            }
        });

    //09 - llamada a la app
    $(function () {
        var app = new UsuariosApp({ usuarios: new Usuarios() });
        app.start();
    });