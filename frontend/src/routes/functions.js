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

export function removeFirst(arr) {
    if (arr.length > 2) {
        arr.shift()
    }
    if (arr.length == 2) {
        arr = [[1]]
    }
    if (arr.length == 1) {
        arr = []
    }
    return arr
}
