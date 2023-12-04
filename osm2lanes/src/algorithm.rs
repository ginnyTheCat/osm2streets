use abstutil::Tags;
use enumset::EnumSet;
use muv_osm::{
    get_tag,
    lanes::{
        lanes,
        travel::{TravelLane, Turn},
        LaneVariant,
    },
    units::{Distance, Unit},
    AccessLevel, Conditional, Lifecycle, TMode, Tag,
};

use crate::{
    osm, BufferType, Direction, DrivingSide, LaneSpec, LaneType, MapConfig, TurnDirection,
};

pub fn get_lane_specs_ltr(tags: &Tags, cfg: &MapConfig) -> Vec<LaneSpec> {
    let highway_type = tags
        .get(osm::HIGHWAY)
        .or_else(|| tags.get("railway"))
        .unwrap();

    let tags: Tag = tags.inner().iter().collect();

    let country = match (cfg.country_code.as_str(), cfg.driving_side) {
        ("", DrivingSide::Left) => "GB",
        ("", DrivingSide::Right) => "US",
        (country, _) => country,
    };

    let lanes = lanes(&tags, &[country]).unwrap();

    fn mode_allowed(val: &Conditional<AccessLevel>) -> bool {
        matches!(
            val.default,
            Some(AccessLevel::Yes | AccessLevel::Designated | AccessLevel::Permissive)
        )
    }

    fn check_mode(
        lane: &TravelLane,
        mode: TMode,
        lt: LaneType,
        forward: bool,
    ) -> Option<(LaneType, Direction)> {
        if lane.forward.access.get(mode).is_some_and(mode_allowed) {
            Some((
                lt,
                if mode == TMode::Foot && !forward {
                    Direction::Back
                } else {
                    Direction::Fwd
                },
            ))
        } else if lane.backward.access.get(mode).is_some_and(mode_allowed) {
            Some((
                lt,
                if mode == TMode::Foot && forward {
                    Direction::Fwd
                } else {
                    Direction::Back
                },
            ))
        } else {
            None
        }
    }

    let middle = lanes.center;
    lanes
        .lanes
        .into_iter()
        .enumerate()
        .map(|(i, lane)| {
            let forward = get_tag!(tags, oneway) == Some("yes")
                || (i * 2 < middle) == (cfg.driving_side == DrivingSide::Left);
            match lane.variant {
                LaneVariant::Travel(t) => {
                    let forward_turns = t.forward.turn.get(TMode::All).and_then(|t| t.default);
                    let backward_turns = t.backward.turn.get(TMode::All).and_then(|t| t.default);

                    let (mut lt, dir) = if forward_turns.is_some_and(|t| t.contains(Turn::Left))
                        && backward_turns.is_some_and(|t| t.contains(Turn::Left))
                    {
                        (LaneType::SharedLeftTurn, Direction::Fwd)
                    } else {
                        check_mode(&t, TMode::LightRail, LaneType::LightRail, forward)
                            .or_else(|| check_mode(&t, TMode::Train, LaneType::LightRail, forward))
                            .or_else(|| {
                                check_mode(&t, TMode::MotorVehicle, LaneType::Driving, forward)
                            })
                            .or_else(|| check_mode(&t, TMode::Bus, LaneType::Bus, forward))
                            .or_else(|| check_mode(&t, TMode::Bicycle, LaneType::Biking, forward))
                            .or_else(|| check_mode(&t, TMode::Foot, LaneType::Sidewalk, forward))
                            .unwrap_or((LaneType::Buffer(BufferType::Stripes), Direction::Fwd))
                    };

                    if lanes.lifecycle == Lifecycle::Construction {
                        lt = LaneType::Construction;
                    }

                    let turns = match dir {
                        Direction::Fwd => forward_turns,
                        Direction::Back => backward_turns,
                    };

                    let mut allowed_turns = EnumSet::default();
                    if let Some(turns) = turns {
                        if turns.contains(Turn::Through) {
                            allowed_turns.insert(TurnDirection::Through);
                        }
                        if turns.contains(Turn::Left) {
                            allowed_turns.insert(TurnDirection::Left);
                        }
                        if turns.contains(Turn::Right) {
                            allowed_turns.insert(TurnDirection::Right);
                        }
                        if turns.contains(Turn::SlightLeft) {
                            allowed_turns.insert(TurnDirection::SlightLeft);
                        }
                        if turns.contains(Turn::SlightRight) {
                            allowed_turns.insert(TurnDirection::SlightRight);
                        }
                        if turns.contains(Turn::SharpLeft) {
                            allowed_turns.insert(TurnDirection::SharpLeft);
                        }
                        if turns.contains(Turn::SharpRight) {
                            allowed_turns.insert(TurnDirection::SharpRight);
                        }
                        if turns.contains(Turn::MergeToLeft) {
                            allowed_turns.insert(TurnDirection::MergeLeft);
                        }
                        if turns.contains(Turn::MergeToRight) {
                            allowed_turns.insert(TurnDirection::MergeRight);
                        }
                        if turns.contains(Turn::Reverse) {
                            allowed_turns.insert(TurnDirection::Reverse);
                        }
                    }

                    LaneSpec {
                        lt,
                        dir,
                        width: lane
                            .width
                            .as_ref()
                            .map(to_geom_distance)
                            .unwrap_or_else(|| {
                                LaneSpec::typical_lane_widths(lt, highway_type)[0].0
                            }),
                        allowed_turns,
                    }
                }
                LaneVariant::Parking(_) => LaneSpec {
                    lt: if lanes.lifecycle == Lifecycle::Construction {
                        LaneType::Construction
                    } else {
                        LaneType::Parking
                    },
                    dir: if forward {
                        Direction::Fwd
                    } else {
                        Direction::Back
                    },
                    width: lane
                        .width
                        .as_ref()
                        .map(to_geom_distance)
                        .unwrap_or_else(|| {
                            LaneSpec::typical_lane_widths(LaneType::Parking, highway_type)[0].0
                        }),
                    allowed_turns: EnumSet::default(),
                },
            }
        })
        .collect()
}

fn to_geom_distance(d: &Unit<Distance>) -> geom::Distance {
    geom::Distance::meters(d.to(Distance::Metre).value as f64)
}
