for (let i = 0; i < 8000; i++) {
    const first = {};
    let last = first;
    for (let j = 0; j < 1000; j++) {
        let next = { prev: last };
        last = next;
    }
}
