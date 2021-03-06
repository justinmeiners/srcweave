func map<S, T>(_ list: [S], _ f: (S) -> T) -> [T] {
    var result: [T] = []
    for x in list {
        result.append(f(x))
    }
    return result
}
