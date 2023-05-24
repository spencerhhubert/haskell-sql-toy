<script>
    import { getQuery, removeFirst } from './functions.js';
    import { onMount } from 'svelte';
    import Table from './table.svelte';

    let ready = false
    let historical_queries = []
    let historical_results = [{}]
    async function getHistoricalRequests() {
        let res = await fetch(`http://localhost:4000/history`)
        if (!res.ok) {
            throw new Error(`error: ${res.status}`);
            return
        }
        historical_queries = await res.json()
        historical_results = [{}]
        for (let i = 0; i < historical_queries.length; i++) {
            let query = historical_queries[i]
            let result = await getQuery(query)
            historical_results.push(result)
        }
    }
    onMount(async () => {
        await getHistoricalRequests()
        historical_results.shift()
        ready = true
    })
</script>
<h3>Historical requests:</h3>
{#each historical_queries as q, i}
    {#if ready}
        <span>-------------------</span>
        <span><i>{q}</i></span>
        <span>-------------------</span>
        <Table json_arr={historical_results[i]} />
    {:else}
        <p>loading...</p>
    {/if}
{/each}
