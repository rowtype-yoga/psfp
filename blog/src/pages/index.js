import React from 'react'

import SEO from '../components/seo'
import landing from '../../output/Landing'
import wrapper from '../../output/Wrapper'


const Landing = landing.mkLandingPage()

const Wrapper = wrapper.mkWrapper()

const Index = () => {
  return (<Wrapper>
    <SEO title="Rowtype Yoga" />
    <Landing />
  </Wrapper>)
}

export default Index
