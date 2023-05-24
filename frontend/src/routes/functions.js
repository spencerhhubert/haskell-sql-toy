export async function getQuery(query) {
    query = encodeURIComponent(query)
    let res = await fetch(`http://localhost:4000/sql/?query=${query}`)
    if (!res.ok) {
        throw new Error(`error: ${res.status}`);
        return
    }
    res = await res.json()
    return res
}
