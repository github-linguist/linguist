<script>
  let name = '';

  $: console.log('The name is', name);
</script>

<input type="text" placeholder="Name" bind:value={name}>

{#if name}
  <h1>Hello, {name}!</h1>
{:else}
  <h3>Please enter your name.</h3>
{/if}