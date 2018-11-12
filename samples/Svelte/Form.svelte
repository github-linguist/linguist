<form on:submit="createHandler(event)">
  <div>
    <label for="slinky-title">Title</label>
    <input
      bind:value='title'
      id="slinky-title"
      name="title"
      type="text"
    />
  </div>
  <button type="submit">Add Now</button>
  <button type="button" on:click="logState()">Log</button>
</form>

<script>
  const defaultData = {
    title: '',
  }

  export default {
    data: () => defaultData,

    methods: {
      logState() {
        console.log(this.get())
      },

      createHandler(event) {
        event.preventDefault()
        // parse, validate, fire, reset
        const slinky = this.get()
        this.fire('create', slinky)
        this.set(defaultData)
      }
    },
  }
</script>