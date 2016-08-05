/* tslint:disable */
import * as React from 'react'
/* tslint:enable */
import { IObservableValue } from 'mobx'
import { EditorModelClass } from '../editorModel'

type Empty = {}
type CompClass = React.ComponentClass<Empty>

export interface EditorComponent {
    ReactComponentClass: CompClass
}
export type EditorComp = EditorComponent

export interface Selectable {
    isSelected: IObservableValue<boolean>
}

export type SelectableComp = EditorComponent & Selectable

export interface SelectableCompClass extends CompClass {
    new (): SelectableComp
}

export type Editor<SrcType, EditorComponent extends EditorComp> = (src: SrcType) => EditorComponent

export type EditorMake<SrcType, EditorModelType, EditorComponent extends EditorComp> =
    (createEditorModel: (src: SrcType) => EditorModelType) => Editor<SrcType, EditorComponent>

export const ENTER_KEY = 13
export const ESCAPE_KEY = 27
