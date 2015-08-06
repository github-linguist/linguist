<style lang="stylus">
font-stack = Helvetica, sans-serif
primary-color = #999
body
  font 100% font-stack
  color primary-color
</style>

<template lang="jade">
div
  h1 {{msg}}
  comp-a
  comp-b
</template>

<script lang="babel">
import compA from './components/a.vue'
import compB from './components/b.vue'

export default {
  data () {
    return {
      msg: 'Hello from Babel!'
    }
  },
  components: {
    'comp-a': compA,
    'comp-b': compB
  }
}
</script>
