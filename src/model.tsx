import { ClassList, Class } from './metamodel'
import { MMString, MMList } from './wm3'

export const theClasses = new ClassList({ classes: new MMList([]) })

export function createClass(name: string): Class {
    return new Class({ name: new MMString(name) })
}
