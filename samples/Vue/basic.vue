<style>
.red {
  color: #f00;
}
</style>

<template>
<div>
  <h2 v-class="red">{{msg}}</h2>
</div>
</template>

<script>
module.exports = {
  data: function () {
    return {
      msg: 'Hello from Vue!'
    }
  }
}
</script>
