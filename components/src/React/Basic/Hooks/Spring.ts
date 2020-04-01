import { useSpring, animated } from "react-spring";

exports.useSpringImpl = useSpring;

exports.animatedComponentImpl = (name: string) => animated[name];

exports.animatedImpl = animated;
