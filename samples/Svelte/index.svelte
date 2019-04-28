<script>
  import SomeComponent from './somecomonent.svelte';

  export let name = '';

  $: console.log('Your name is', name);
</script>

<input type="text" placeholder="Name" bind:value={name}>

{#if name}
  <h1>Hello, {name}!</h1>
{:else}
  <h3>Please enter your name.</h3>
{/if}

<style>
  h1 {
    font-weight: 600;
    color: #0a0a0a;
  }

  h3 {
    color: gray;
  }
</style>