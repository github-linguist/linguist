// Fixture taken from https://github.com/graphcool/console/blob/dev/src/components/onboarding/PlaygroundCPopup/PlaygroundCPopup.tsx

import * as React from 'react'
import {withRouter} from 'react-router'
import {connect} from 'react-redux'
import {bindActionCreators} from 'redux'
import {nextStep, selectExample} from '../../../actions/gettingStarted'
import {classnames} from '../../../utils/classnames'
import Loading from '../../Loading/Loading'
import {GettingStartedState} from '../../../types/gettingStarted'
import {Example} from '../../../types/types'
const classes: any = require('./PlaygroundCPopup.scss')
import {$p} from 'graphcool-styles'
import * as cx from 'classnames'

interface Tutorial {
  title: string
  description: string
  image: string
  link: string
}

const guides: Tutorial[] = [
  {
    title: 'Learnrelay.org',
    description: 'A comprehensive, interactive introduction to Relay',
    link: 'https://learnrelay.org/',
    image: require('../../../assets/graphics/relay.png'),
  },
  {
    title: 'GraphQL and the amazing Apollo Client',
    description: 'Explore an Application built using React and Angular 2',
    link: 'https://medium.com/google-developer-experts/graphql-and-the-amazing-apollo-client-fe57e162a70c',
    image: require('../../../assets/graphics/apollo.png'),
  },
  {
    title: 'Introducing Lokka',
    description: 'A Simple JavaScript Client for GraphQL',
    link: 'https://voice.kadira.io/introducing-lokka-a-simple-javascript-client-for-graphql-e0802695648c',
    image: require('../../../assets/graphics/lokka.png'),
  },
]

const examples = {
  ReactRelay: {
    path: 'react-relay-instagram-example',
    description: 'React + Relay',
  },
  ReactApollo: {
    path: 'react-apollo-instagram-example',
    description: 'React + Apollo',
  },
  ReactNativeApollo: {
    path: 'react-native-apollo-instagram-example',
    description: 'React Native + Apollo',
  },
  AngularApollo: {
    path: 'angular-apollo-instagram-example',
    description: 'Angular + Apollo',
  },
}

interface Props {
  id: string
  projectId: string
  nextStep: () => Promise<void>
  selectExample: (selectedExample: Example) => any
  gettingStartedState: GettingStartedState
}

interface State {
  mouseOver: boolean
}

class PlaygroundCPopup extends React.Component<Props, State> {

  state = {
    mouseOver: false,
  }

  refs: {
    [key: string]: any
    exampleAnchor: HTMLDivElement
    congratsAnchor: HTMLDivElement
    scroller: HTMLDivElement,
  }

  componentDidUpdate(prevProps: Props, prevState: State) {
    if (prevProps.gettingStartedState.selectedExample !== this.props.gettingStartedState.selectedExample) {
      this.refs.scroller.scrollTop += this.refs.exampleAnchor.getBoundingClientRect().top
    }

    if (prevProps.gettingStartedState.isCurrentStep('STEP5_WAITING')
        && this.props.gettingStartedState.isCurrentStep('STEP5_DONE')) {
      this.refs.scroller.scrollTop += this.refs.congratsAnchor.getBoundingClientRect().top

      const snd = new Audio(require('../../../assets/success.mp3') as string)
      snd.volume = 0.5
      snd.play()
    }
  }

  render() {
    const {mouseOver} = this.state
    const {selectedExample} = this.props.gettingStartedState
    const hovering = !this.props.gettingStartedState.isCurrentStep('STEP4_CLICK_TEASER_STEP5')
    const downloadUrl = (example) => `${__BACKEND_ADDR__}/resources/getting-started-example?repository=${examples[example].path}&project_id=${this.props.projectId}&user=graphcool-examples` // tslint:disable-line
    return (
      <div
        className='flex justify-center items-start w-100 h-100'
        style={{
          transition: 'background-color 0.3s ease',
          backgroundColor: hovering ? 'rgba(255,255,255,0.7)' : 'transparent',
          width: 'calc(100% - 266px)',
          pointerEvents: 'none',
          overflow: 'hidden',
        }}
      >
        <div
          ref='scroller'
          className='flex justify-center w-100'
          style={{
            transition: 'height 0.5s ease',
            height: hovering ? '100%' : mouseOver ? '190%' : '210%',
            pointerEvents: hovering ? 'all' : 'none',
            cursor: hovering ? 'auto' : 'pointer',
            overflow: hovering ? 'auto' : 'hidden',
            alignItems: selectedExample ? 'flex-start' : 'center',
          }}
        >
          <div
            className='bg-white br-2 shadow-2 mv-96'
            style={{
              minWidth: 600,
              maxWidth: 800,
              pointerEvents: 'all',
            }}
            onMouseLeave={() => this.setState({ mouseOver: false })}
            onMouseEnter={() => {
              this.setState({ mouseOver: true })
            }}
            onMouseOver={(e: any) => {
              if (this.props.gettingStartedState.isCurrentStep('STEP4_CLICK_TEASER_STEP5')) {
                this.props.nextStep()
              }
            }}
          >
            <div className='ma-16 tc pb-25'>
              <div className='fw3 ma-38 f-38'>
                You did it! Time to run an example.
              </div>
              <div className='fw2 f-16 mh-96 lh-1-4'>
                You have successfully set up your own Instagram backend.{' '}
                When building an app with Graphcool you can easily explore queries in the{' '}
                playground and "copy &amp; paste" selected queries into your code.{' '}
                Of course, to do so, you need to implement the frontend first.
              </div>
              <div className='fw2 f-16 mh-96 lh-1-4 mt-16'>
                <div>We put together a simple app to show and add posts</div>
                <div>using the backend you just built, to test and run it locally.</div>
              </div>
            </div>
            <div className='ma-16 tc pb-25'>
              <div className='fw3 ma-38 f-25'>
                Select your preferred technology to download the example.
              </div>
              <div className='flex justify-between items-center w-100' ref='exampleAnchor'>
                <div
                  className={classnames(
                    classes.exampleButton,
                    selectedExample === 'ReactRelay' ? classes.active : '',
                  )}
                  onClick={() => this.props.selectExample('ReactRelay')}
                >
                  React + Relay
                </div>
                <div
                  className={classnames(
                    classes.exampleButton,
                    selectedExample === 'ReactApollo' ? classes.active : '',
                  )}
                  onClick={() => this.props.selectExample('ReactApollo')}
                >
                  React + Apollo
                </div>
                <div
                  className={classnames(
                    classes.exampleButton,
                    selectedExample === 'ReactNativeApollo' ? classes.active : '',
                  )}
                  onClick={() => this.props.selectExample('ReactNativeApollo')}
                >
                  React Native + Apollo
                </div>
                <div
                  className={classnames(
                    classes.exampleButton,
                    selectedExample === 'AngularApollo' ? classes.active : '',
                  )}
                  onClick={() => this.props.selectExample('AngularApollo')}
                >
                  Angular + Apollo
                </div>
              </div>
            </div>
          {selectedExample &&
          <div>
            <div className='w-100'>
              <iframe
                className='w-100'
                height='480'
                allowFullScreen
                frameBorder='0'
                src={`https://www.youtube.com/embed/${this.getExampleVideoUrl(selectedExample)}`}
              />
            </div>
            <div
              className='w-100 pa-25'
              style={{
                backgroundColor: '#FEF5D2',
              }}
            >
              <div className='mt-25 mb-38 w-100 flex justify-center'>
                <a
                  href={downloadUrl(selectedExample)}
                  className='pa-16 white'
                  style={{
                    backgroundColor: '#4A90E2',
                  }}
                >
                  Download example
                </a>
              </div>
              <div className='code dark-gray'>
                <div className='black-50'>
                  # To see the example in action, run the following commands:
                </div>
                <div className='mv-16'>
                  <div className='black'>
                    npm install
                  </div>
                  <div className='black'>
                    npm start
                  </div>
                </div>
                <div className='black-50'>
                  # You can now open the app on localhost:3000
                </div>
                <div className='black-50'>
                  # Please come back to this page once you're done. We're waiting here. 💤
                </div>
                <div className={cx($p.w100, $p.flex, $p.flexRow, $p.justifyCenter, $p.mt25)}>
                  <a href='#' onClick={
                    (e: any) => {
                      e.preventDefault()
                      // we need to skip the 'STEP5_WAITING' step
                      this.props.nextStep()
                      this.props.nextStep()
                      this.props.nextStep()
                    }
                  }>
                    Skip
                  </a>
                </div>
              </div>
              {this.props.gettingStartedState.isCurrentStep('STEP5_WAITING') &&
              <div className='w-100 mv-96 flex justify-center'>
                <Loading />
              </div>
              }
            </div>
          </div>
          }
        {this.props.gettingStartedState.isCurrentStep('STEP5_DONE') &&
          <div className='w-100 mb-96' ref='congratsAnchor'>
            <div className='flex items-center flex-column pv-60 fw1'>
              <div className='f-96'>
                🎉
              </div>
              <div className='f-38 mt-38'>
                Congratulations!
              </div>
              <div className='f-38 mt-16'>
                We knew you had it in you.
              </div>
              <div className='f-16 mv-38'>
                Now go out there and build amazing things!
              </div>
            </div>
            <div className='flex justify-between ph-25 pv-16'>
              <div className='w-50 pr-16'>
                <div className='ttu ls-2 f-16 fw1 lh-1-4'>
                  Get started on your own<br />with those excellent tutorials
                </div>
                <div className='mv-38'>
                  {guides.map(guide => this.renderBox(guide))}
                </div>
              </div>
              <div className='w-50 pl-16'>
                <div className='ttu ls-2 f-16 fw1 lh-1-4'>
                  Get more out of Graphcool<br />with our guides
                </div>
                <div className={`h-100 justify-start flex flex-column mv-38 ${classes.guides}`}>
                  <a
                    href='https://graph.cool/docs/tutorials/quickstart-2-daisheeb9x'
                    className={`${classes.one} fw4 black db flex items-center mb-25`}
                    target='_blank'
                  >
                    Declaring Relations
                  </a>
                  <a
                    href='https://graph.cool/docs/tutorials/quickstart-3-saigai7cha'
                    className={`${classes.two} fw4 black db flex items-center mb-25`}
                    target='_blank'
                  >
                    Implementing Business Logic
                  </a>
                  <a
                    href='https://graph.cool/docs/tutorials/thinking-in-terms-of-graphs-ahsoow1ool'
                    target='_blank'
                    className={`${classes.three} fw4 black db flex items-center mb-25`}
                  >
                    Thinking in terms of graphs
                  </a>
                </div>
              </div>
            </div>
            <div className='flex w-100 justify-center'>
              <div
                className='f-25 mv-16 pv-16 ph-60 ls-1 ttu pointer bg-accent white dim'
                onClick={this.props.nextStep}
              >
                Finish Onboarding
              </div>
            </div>
          </div>
          }
          </div>
        </div>
      </div>
    )
  }

  private renderBox = (tutorial: Tutorial) => {
    return (
      <div key={tutorial.title} className='pa-16 mb-16 lh-1-4' style={{background: 'rgba(0,0,0,0.03)'}}>
        <a className='flex items-center' href={tutorial.link} target='_blank'>
          <div className='flex items-center justify-center' style={{ flex: '0 0 60px', height: 60 }}>
            <img src={tutorial.image}/>
          </div>
          <div className='flex flex-column space-between ml-38'>
            <div className='mb-6 dark-gray f-16'>
              {tutorial.title}
            </div>
            <div className='fw1 mid-gray'>
              {tutorial.description}
            </div>
          </div>
        </a>
      </div>
    )
  }

  private getExampleVideoUrl = (example: Example): string => {
    switch (example) {
      case 'ReactRelay': return '_dj9Os2ev4M'
      case 'ReactApollo': return '9nlwyPUPXjQ'
      case 'ReactNativeApollo': return '9nlwyPUPXjQ'
      case 'AngularApollo': return 'EzD5fJ-uniI'
    }
  }
}

const mapStateToProps = (state) => {
  return {
    gettingStartedState: state.gettingStarted.gettingStartedState,
  }
}

const mapDispatchToProps = (dispatch) => {
  return bindActionCreators({nextStep, selectExample}, dispatch)
}

export default connect(mapStateToProps, mapDispatchToProps)(withRouter(PlaygroundCPopup))
