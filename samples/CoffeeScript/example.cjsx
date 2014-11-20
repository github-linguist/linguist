###* @cjsx React.DOM ###
define 'myProject.ReactExampleComponent', [
  'React'
  'myProject.ExampleStore'
  'myProject.ExampleActions'
  'myProject.ReactExampleTable'
], (React, ExampleStore, ExampleActions, ReactExampleTable ) ->

  ReactExampleComponent = React.createClass
    mixins: [ListenMixin]

    getInitialState: ->
      rows: ExampleStore.getRows()
      meta: ExampleStore.getMeta()

    componentWillMount: ->
      @listenTo ExampleStore

    componentDidMount: ->
      ExampleActions.getExampleData()

    onStoreChange: ->
      if this.isMounted()
        @setState
          rows: ExampleStore.getRows()
          meta: ExampleStore.getMeta()

    componentWillUnmount: ->
      @stopListening ExampleStore

    render: ->
      <div className="page-wrap">
          <header>
            <strong> {@state.title} </strong>
          <header>
            <ReactExampleTable
              rows={@state.rows},
              meta={@state.meta}
            />
      </div>
