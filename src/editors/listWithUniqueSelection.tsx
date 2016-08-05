/* tslint:disable */
import * as React from 'react'
/* tslint:enable */
import { observer } from 'mobx-react'
import { autorun, computed, observable } from 'mobx'
import { EditorModelClass } from '../editorModel'
import { MMElement } from '../wm3'
import { Editor, SelectableComp, EditorComp, ENTER_KEY } from './common'

interface ListEditorModel<EltType> {
    get(idx: number): EltType
    add(idx: number, elt: EltType): void
    length(): number
    remove(idx: number): void
}

/**
 * Note that [createListEditorModel] creates a list editor model whose elements are *source elements*,
 * and not editor models.
 */
export function listWithUniqueSelection<SrcType, EltSrcType>
    (createListEditorModel: (src: SrcType) => ListEditorModel<EltSrcType>,
    itemEditor: Editor<EltSrcType, SelectableComp>): Editor<SrcType, EditorComp> {
    return (src: SrcType): EditorComp => {
        const editorModel = createListEditorModel(src)
        const newItemUIValue = observable('')
        const children = computed(() => editorModel.get().map(subEditor))
        autorun(() => {
            for (let child of children.get()) {
                autorun(() => {
                    if (child.isSelected.get()) {
                        children.get().filter(i => i !== child).forEach(i => i.isSelected.set(false))
                    }
                })
            }
        })

        function handleChange(event: React.FormEvent) {
            newItemUIValue.set((event.target as any).value)
        }

        function handleKeyUp(event: React.KeyboardEvent) {
            if (event.keyCode === ENTER_KEY) {
                // ICI: also possibility to:
                // - create an editor model from nothing/some string
                // - create a source element from an editor model
                // ?
                editorModel.add(editorModel.length(), newItemUIValue.get())
                newItemUIValue.set('')
            }
        }

        const ReactComponentClass = observer(() => {
            return <div>
                <ul>
                    <li><input onChange={handleChange} onKeyUp={handleKeyUp} value={newItemUIValue.get() } autoFocus={true}/></li>
                    {children.get().map(renderItem) }</ul>
            </div>
        })

        function renderItem(item: EditorComp, idx: number) {
            return <li key={editorModel.get(idx).key}>
                <item.ReactComponentClass/>
                <button type='button' onClick={() => editorModel.remove(idx) }>-</button>
            </li>
        }

        return { ReactComponentClass }
    }
}
