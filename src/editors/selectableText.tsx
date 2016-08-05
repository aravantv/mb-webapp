/* tslint:disable */
import * as React from 'react'
/* tslint:enable */
import { observer } from 'mobx-react'
import { autorun, observable, computed } from 'mobx'
import { EditorModelClass } from '../editorModel'
import { MMElement, MMString } from '../wm3'
import { SelectableComp, Editor, ENTER_KEY, ESCAPE_KEY } from './common'

interface TextEditorModel {
    getText(): string
    setText(val: string): void
}

function textEditorModel_of_domainModel(model: MMString) {
    return class implements TextEditorModel {
        public getText() {
            return model.content
        }
        public setText(val: string) {
            model.content = val
        }
    }
}

export function selectableTextMake<SrcType>
    (createTextEditorModel: (src: SrcType) => TextEditorModel): Editor<SrcType, SelectableComp> {

    return (src: SrcType): SelectableComp => {
        const editorModel = createTextEditorModel(src)
        const isSelected = observable(false)
        const uiValue = observable('')

        autorun(() => {
            if (isSelected.get()) {
                uiValue.set(editorModel.getText())
            }
        })

        function handleChange(event: React.FormEvent) {
            uiValue.set((event.target as any).value)
        }

        function handleKeyUp(event: React.KeyboardEvent) {
            const isEnter = event.keyCode === ENTER_KEY
            if (isEnter || event.keyCode === ESCAPE_KEY) {
                isSelected.set(false)
                if (isEnter) {
                    editorModel.setText(uiValue.get())
                }
            }
        }

        function handleDoubleClick(event: React.MouseEvent) {
            isSelected.set(true)
        }

        let ReactComponentClass = observer(
            () => isSelected.get()
                ? <input onChange={handleChange} onKeyUp={handleKeyUp} value={uiValue.get() }/>
                : <span onDoubleClick={handleDoubleClick}>{editorModel.getText() }</span>
        )
        return { ReactComponentClass, isSelected }
    }
}
