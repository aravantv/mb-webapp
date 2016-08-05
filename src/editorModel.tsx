import { MMElement } from './wm3'

/** For editors which are disconnected from any metamodel element. */
export interface EditorModelClass<EditorModel> {
    new (): EditorModel
}

/** For editors which allow edition of a metamodel element (the "source"). */
export interface DomainBasedEditorModelClass<DomainModelType extends MMElement<any>, EditorModel> {
    new (src: DomainModelType): EditorModel
}