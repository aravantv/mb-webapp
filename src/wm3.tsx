import * as UUID from 'node-uuid'
import { observable } from 'mobx'

export type UUID = string

function getNewID(): UUID {
    return UUID.v1()
}

export interface MMElement<T> {
    uuid: UUID
    content: T
}

export class MMElementBase<T> implements MMElement<T> {
    public uuid: UUID
    @observable
    public content: T
    constructor(content: T) {
        this.uuid = getNewID()
        this.content = content
    }
}

export class MMString extends MMElementBase<string> { }

export class MMList<T extends MMElement<any>> extends MMElementBase<T[]> { }

type UUIDRef = { refUUID: UUID }

export interface MMRef<T> extends MMElement<UUIDRef> {
    get(onGet: (val: T) => void): void
}

export class LocalRef<T> implements MMRef<T> {
    constructor(public uuid: UUID, public ref: MMElement<T>) { }

    get content() {
        return { refUUID: this.ref.uuid }
    }

    public get(onGet: (val: T) => void): void {
        onGet(this.ref.content)
    }
}

export type UUIDConstructor<T> = (elt: T) => UUID
