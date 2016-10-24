import { isEnter, isEscape, getText, extend } from './lib'
import { Stream } from 'xstream'
import { span, input, VNode } from '@cycle/dom'
import { DOMSource } from '@cycle/dom/xstream-typings'

export interface RawInput {
  dom: DOMSource
}

export interface Output {
  dom: Stream<VNode>,
  confirmed$: Stream<string>,
}

export interface Input {
  confirm$: Stream<void>,
  cancel$: Stream<void>,
  select$: Stream<void>,
  change$: Stream<string>,
  initUIValue?: string,
  initValidatedValue?: string,
  initIsSelected?: boolean,
}

interface IState {
  uiValue: string,
  validatedName: string,
  isSelected: boolean,
}

export function keyMousePreprocessor(i: RawInput): Input {
  const keyups$ = i.dom.select('.field').events('keyup')
  return {
    confirm$: keyups$.filter(isEnter).mapTo(null),
    cancel$: keyups$.filter(isEscape).mapTo(null),
    change$: keyups$.filter(ev => !isEscape(ev) && !isEnter(ev)).map(getText),
    select$: i.dom.select('.field').events('dblclick').mapTo(null),
  }
}

function react$(i: Input, initState: IState): Stream<IState> {
  return Stream.merge(
    i.confirm$.mapTo(s => extend({ validatedName: s.uiValue, isSelected: false }, s)),
    i.cancel$.mapTo(s => extend({ uiValue: s.validatedName, isSelected: false }, s)),
    i.change$.map(n => s => extend({ uiValue: n }, s)),
    i.select$.mapTo(s => extend({ isSelected: true }, s))
  ).fold((acc, f: (prev: IState) => IState) => f(acc), initState)
}

function render$(s$: Stream<IState>): Stream<VNode> {
  return s$.map(s =>
    s.isSelected
      ? input('.field', { props: { type: 'text', value: s.uiValue } })
      : span('.field', s.validatedName))
}

export function SelectableText(i: Input): Output {
  const isSelected = i.initIsSelected !== null ? i.initIsSelected : true
  const uiValue = i.initUIValue ? i.initUIValue : ''
  const validatedName = i.initValidatedValue
  const state$ = react$(i, { isSelected, uiValue, validatedName })
  return {
    dom: render$(state$),
    confirmed$: state$.map(s => i.confirm$.map(() => s.uiValue)).flatten(),
  }
}
