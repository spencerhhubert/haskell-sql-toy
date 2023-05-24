<script>
    import Table from './table.svelte';
    import { getQuery } from './functions.js';

    let result = [{}]
    async function handleSubmit() {
        let query = document.getElementById("input_query").value
        result = await getQuery(query)
    }
</script>

<h3>Make an SQL query on the JSON file</h3>
<input id="input_query" type="text" placeholder="sql query" />
<button on:click={handleSubmit}>Submit</button>
<h3>Result:</h3>
<Table json_arr={result} />
