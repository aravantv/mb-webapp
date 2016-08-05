export interface WithKey {
    key: string
}

export function named<T>(x: T, name: string): T {
    (x as any).name = name
    return x
}

export function namedObserver<T>(x: T, name: string): T {
    (x as any).displayName = name
    return x
}

export class Ref<T> {
    constructor(public content: T) { }
}

interface Indexed {
    [key: string]: any
}

/** Merges [first] and [second]. In case a field exists in both parameters, the Fields of [first] have priority. */
export function extend<T extends Indexed, U extends Indexed>(first: T, second: U): T & U {
    let result = {} as T & U
    for (let id in first) {
        result[id] = first[id]
    }
    for (let id in second) {
        if (!result.hasOwnProperty(id)) {
            result[id] = second[id]
        }
    }
    return result
}

export class Option<T> {
    private none: boolean
    private value: T

    constructor(none: boolean, value: T) {
        this.none = none
        this.value = value
    }

    public getValue(): T {
        return this.value
    }

    public isNone(): boolean {
        return this.none
    }

    public is(val: T) {
        return !this.none && this.value === val
    }
}

export const none = new Option(true, null)

export function some<T>(value: T): Option<T> {
    return new Option(false, value)
}

export const ESCAPE_KEY = 27
export const ENTER_KEY = 13
