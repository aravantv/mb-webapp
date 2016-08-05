import { MMString, MMList, MMElementBase } from './wm3'

// Metamodel

export class Class extends MMElementBase<{ name: MMString }> { }

export class ClassList extends MMElementBase<{ classes: MMList<Class> }> { }

