<script>
    export let json_arr = []
    let cols = []
    let rows = []
    $: {
        cols = Object.keys(json_arr[0]) || [];
        rows = json_arr.map(obj => Object.values(obj)) || [];
    }
</script>

<table>
  <thead>
    <tr>
      {#each cols as col}
        <th>{col}</th>
      {/each}
    </tr>
  </thead>
  <tbody>
    {#each rows as row}
      <tr>
        {#each row as cell}
          <td>{cell}</td>
        {/each}
      </tr>
    {/each}
  </tbody>
</table>

