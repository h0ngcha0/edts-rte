(function($) {
    var app = null;
    var zIndex = 1;

    var debug = function(Arg){
        console.log(Arg);
    };

    var rand = function(upper){
        return Math.floor(Math.random()*upper) + 50
    };

    var sample_code = "expand_recs(M,List) when is_list(List) ->"
            + "[expand_recs(M,L)||L<-List];";

    var CodeEditorModel = Backbone.Model.extend({
        defaults: function() {
            return {
                id: 0,
                code: sample_code,
                x: rand(50),
                y: rand(50),
                z: zIndex
            }
        },

        initialize: function(spec) {
            this.set('id', spec.id);
            this.set('code', spec.code);
        },

        toJSON: function() {
            return {
                x: this.get('x'),
                y: this.get('y'),
                z: this.get('z'),
                id: this.get('id'),
                code: this.get('code')
            }
        },

        clear: function() {
            this.destroy();
        }
    });

    var CodeEditorList = Backbone.Collection.extend({
        url: "rte/editors",
        model: CodeEditorModel
    });

    var code_editors = new CodeEditorList;

    var CodeEditorView = Backbone.View.extend({
        initialize: function() {
            this.html = app.renderTemplate('code_editor', this.model.toJSON());
        },

        events: {
            "submit": "delete"
        },

        render: function() {
            self = this;
            var id = this.model.get('id');
            var textarea_id = "code_editor_"+id;
            $(this.el).html(this.html);

            self.$("#"+textarea_id).ready(function() {
                debug(textarea_id+" ready!");
                debug(self.$("#"+textarea_id)[0]);
                var editor = CodeMirror.fromTextArea(self.$("#"+textarea_id)[0], {
                    lineNumbers: true,
                    matchBrackets: true,
                    extraKeys: {"Tab":  "indentAuto"},
                    theme: "erlang-dark",
                });
                // FIXME: I have no idea why we will have to call a refresh, but
                // otherwise codemirror will only show the code when a key is pressed
                setTimeout(editor.refresh, 0);

                var codecontainer_id = "code_container_"+id;
                self.$("div."+codecontainer_id).draggable({
                    opacity: 0.75,
                    //cancel: '.slider',
                    stop: function(e, ui) {
                        debug("in draggable callback")
                    }
                });
            });
            return this;
        },

        delete: function(e) {
            e.preventDefault();
            debug("delete editor");
            this.model.clear();
        }
    });

    var CodeEditorListView = Backbone.View.extend({
        render: function() {
            var self = this;
            self.$el.empty();
            _.forEach(self.model.models, function(ce, i) {
                self.$el.append(new CodeEditorView({model: ce}).render().el);
            });
            return self;
        }
    });

    var InputView = Backbone.View.extend({
        tagName: "form",

        initialize: function() {
            this.editor = undefined;
        },

        events: {
            "submit": "submit"
        },

        render: function() {
            self = this;
            this.$el.html(app.renderTemplate('code_input', {}));
            self.$("#code_input").ready(function() {
                self.editor = CodeMirror.fromTextArea(self.$("#code_input")[0], {
                    lineNumbers: true,
                    matchBrackets: true,
                    extraKeys: {"Tab":  "indentAuto"},
                    theme: "erlang-dark",
                });

                setTimeout(self.editor.refresh, 0);
            });
            return this;
        },

        submit: function(e) {
            e.preventDefault();
            var id = this.$("#id_input").val();
            debug("creating code editor:");
            debug(id);
            code_editors.create({ id: id,
                                  code: this.editor.getValue()});
        }
    });

    AppView = Backbone.View.extend({
        el: $("#rett"),
        cache: {},

        initialize: function() {
            app = this;

            // listen to the 'all' events of the code editors
            code_editors.on('all', this.render, this);

            // fetch all the editors from server
            code_editors.fetch({
                success: function(c, r, o) {
                    debug("fetch editors success!");
                },
                error: function() {
                    debug("fetch editors error!");
                }
            }).complete(function() {
                debug("fetch editors done!");
            });
        },

        loadTemplate: function(name) {
            var url = "rte/static/views/" + name + ".handlebars";
            var template = $.ajax({url: url, async: false}).responseText;
            return Handlebars.compile(template);
        },

        renderTemplate: function(name, data) {
            var self = this;
            if(!self.cache[name]) {
                self.cache[name] = self.loadTemplate(name);
            }
            return self.cache[name](data || {});
        },

        render: function() {
            var editorsView = new CodeEditorListView({model: code_editors});
            var inputView = new InputView();

            debug("rendering");

            $('#code-editors').empty();
            $('#code-editors').append(editorsView.render().el);
            $('#code-editors').append(inputView.render().el);
            var newTitle = "Rett";
            var title = "Code Editor";
            if(!!title && title.length > 0) {
                newTitle += " - " + title;
            }
            document.title = newTitle;
            window.prettyPrint && prettyPrint();
            return this;
        },
    });
})(jQuery);
