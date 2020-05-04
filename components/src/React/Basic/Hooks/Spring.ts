import {
  useChain,
  useSpring,
  useSprings,
  useTransition,
  interpolate,
  animated,
} from "react-spring";

exports.interpolateImpl = interpolate;

exports.useSpringImpl = (mkStyles) => () => {
  const result = useSpring(mkStyles);
  return { style: result[0], set: result[1], stop: result[2] };
};

exports.useSpringsImpl = (n: number) => (fn) => () => {
  const result = useSprings(n, fn);
  return { styles: result[0], set: result[1], stop: result[2] };
};

exports.useTransitionImpl = useTransition;

exports.animatedComponentImpl = (name: string) => animated[name];

exports.animatedImpl = animated;
