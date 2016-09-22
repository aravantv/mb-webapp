export const ENTER_KEY = 13
export const ESCAPE_KEY = 27

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

export const isEnter = (ev: Event) => (ev as KeyboardEvent).keyCode === ENTER_KEY
export const isEscape = (ev: Event) => (ev as KeyboardEvent).keyCode === ESCAPE_KEY
export const getText = (ev: Event) => (ev.target as HTMLInputElement).value
