<script>
    import Table from './table.svelte';

    let result = [{}]
    async function handleSubmit() {
        let query = document.getElementById("input_query").value
        query = encodeURIComponent(query)
        let res = await fetch(`http://localhost:4000/sql/?query=${query}`)
        if (!res.ok) {
            throw new Error(`HTTP error! status: ${res.status}`);
            return
        }
        res = await res.json()
        console.log(res)
        result = res
    }
</script>

<input id="input_query" type="text" placeholder="sql query" />
<button on:click={handleSubmit}>Submit</button>
<Table json_arr={result} />
